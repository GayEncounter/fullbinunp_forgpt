#include "physx2\NxPhysics.h"
#include "common.cpp"

static NxPhysicsSDK *pSDK = NULL;

#pragma comment(linker, "/export:PHInitialize@0=PHInitialize")
int __stdcall PHInitialize(void)
{
	static MyNxErrorStream errorStream;

	if(!pSDK)
	{
		pSDK = NxCreatePhysicsSDK(NX_PHYSICS_SDK_VERSION);
		if(!pSDK)
			return 0;
			
		NxFoundationSDK *foundation = &pSDK->getFoundationSDK();
		foundation->setErrorStream(&errorStream);
			
		InitCooking();
			
		return 1;
	}
	else
		return 1; // already initialized
}

#pragma comment(linker, "/export:PHFinalize@0=PHFinalize")
void __stdcall PHFinalize(void)
{
	if(pSDK)
	{
		pSDK->release();
		pSDK = NULL;
		
		CloseCooking();
	}
}

#pragma comment(linker, "/export:PHCreateTriMesh@28=PHCreateTriMesh")
NxTriangleMesh * __stdcall PHCreateTriMesh(int numVerts, int numTriangles, void *pVerts,
	void *pTris, int vertStride, int triangleStride, int flags)
{
	if(!pSDK)
		return NULL;

	void *data;
	int sz;

	int ret = CookTriangleMesh(numVerts, numTriangles, pVerts, pTris, 
		vertStride, triangleStride, flags, &data, &sz);

	if(!ret)
		return 0;

	MyNxMemReader st(data, sz);
	NxTriangleMesh *tm = pSDK->createTriangleMesh(st);
	return tm;
}

#pragma comment(linker, "/export:PHLoadTriMesh@8=PHLoadTriMesh")
NxTriangleMesh * __stdcall PHLoadTriMesh(void *data, int size)
{
	MyNxMemReader st(data, size);
	NxTriangleMesh *tm = pSDK->createTriangleMesh(st);
	return tm;
}

#pragma comment(linker, "/export:PHFreeTriMesh@4=PHFreeTriMesh")
void __stdcall PHFreeTriMesh(NxTriangleMesh *mesh)
{
	pSDK->releaseTriangleMesh(*mesh);
}

#pragma comment(linker, "/export:PHCreateScene@0=PHCreateScene")
NxScene * __stdcall PHCreateScene(void)
{
	if(pSDK)
	{
		NxSceneDesc sd;
		NxScene *pScene = pSDK->createScene(sd);
		
		return pScene;
	}
	else
		return NULL;
}

#pragma comment(linker, "/export:PHDestroyScene@4=PHDestroyScene")
void  __stdcall PHDestroyScene(NxScene *pScene)
{
	if(pSDK && pScene)
		pSDK->releaseScene(*pScene);
}

#pragma comment(linker, "/export:PHCullShapes@24=PHCullShapes")
int __stdcall PHCullShapes(NxScene *pScene, int num_planes, float *planes, int num_shapes, NxShape **shapes, unsigned group)
{
	printf("num_planes = %d num_shapes = %d groups = %d\n", num_planes, num_shapes, group);
	std::vector<NxPlane> nxplanes;
	nxplanes.resize(num_planes);
	for(int i = 0; i < num_planes; i++)
	{
		nxplanes[i].set(planes[0], planes[1], planes[2], planes[3]);
		nxplanes[i].normalize();
		planes += 4;
	}
	
	return pScene->cullShapes(nxplanes.size(), &nxplanes[0], NX_ALL_SHAPES, num_shapes, shapes, NULL, group);
}

#pragma comment(linker, "/export:PHRaycastAnyShape@20=PHRaycastAnyShape")
int __stdcall PHRaycastAnyShape(NxScene *pScene, float *p, float *dir, float dist, unsigned group)
{
	NxRay ray(NxVec3(p[0], p[1], p[2]), NxVec3(dir[0], dir[1], dir[2]));
	if(pScene->raycastAnyShape(ray, NX_ALL_SHAPES, group, dist))
		return 1;

	return 0;
}

#pragma comment(linker, "/export:PHRaycastClosestShape@32=PHRaycastClosestShape")
NxShape * __stdcall PHRaycastClosestShape(NxScene *pScene, float *p, float *dir, float dist, float *hit_pos, float *hit_nrm, float *uv, unsigned group)
{
	NxShape *shape;

	NxRay ray(NxVec3(p[0], p[1], p[2]), NxVec3(dir[0], dir[1], dir[2]));
	//NxRay ray(NxVec3(0, 50, 0), NxVec3(0, -1, 0));
	NxRaycastHit hit;

	//printf("%f %f %f\n%f %f %f\n", p[0], p[1], p[2], dir[0], dir[1], dir[2]);  

	memset(&hit, 0, sizeof(hit));
	hit.flags = NX_RAYCAST_SHAPE;
	if(hit_pos)
		hit.flags |= NX_RAYCAST_IMPACT;
	if(hit_nrm)
		hit.flags |= NX_RAYCAST_NORMAL;
	if(uv)
		hit.flags |= NX_RAYCAST_UV;

	shape = pScene->raycastClosestShape(ray, NX_ALL_SHAPES, hit, group, dist);

	if(shape)
	{
		if(hit_pos)
		{
			hit_pos[0] = hit.worldImpact.x;
			hit_pos[1] = hit.worldImpact.y;
			hit_pos[2] = hit.worldImpact.z;
		}

		if(hit_nrm)
		{
			hit_nrm[0] = hit.worldNormal.x;
			hit_nrm[1] = hit.worldNormal.y;
			hit_nrm[2] = hit.worldNormal.z;
		}

		if(uv)
		{
			uv[0] = hit.u;
			uv[1] = hit.v;
		}

		return shape;
	}

	return NULL;
}

#pragma comment(linker, "/export:PHRaycastClosestActor@32=PHRaycastClosestActor")
NxActor * __stdcall PHRaycastClosestActor(NxScene *pScene, float *p, float *dir, float dist, float *hit_pos, float *hit_nrm, float *uv, unsigned group)
{
	void *shape = PHRaycastClosestShape(pScene, p, dir, dist, hit_pos, hit_nrm, uv, group);
	if(shape)
		return &((NxShape*)shape)->getActor();
	else
		return 0;
}

#pragma comment(linker, "/export:PHShapeBox@8=PHShapeBox")
void* __stdcall PHShapeBox(float *dim, float *offset)
{
	NxBoxShapeDesc *sd = new NxBoxShapeDesc;
	sd->dimensions.x = dim[0];
	sd->dimensions.y = dim[1];
	sd->dimensions.z = dim[2];

	if(offset)
	{
		sd->localPose.t.x = offset[0];
		sd->localPose.t.y = offset[1];
		sd->localPose.t.z = offset[2];
	}

	return sd;
}

#pragma comment(linker, "/export:PHShapeBoxM44@8=PHShapeBoxM44")
void* __stdcall PHShapeBoxM44(float *dim, float *mat)
{
	NxBoxShapeDesc *sd = new NxBoxShapeDesc;
	sd->dimensions.x = dim[0];
	sd->dimensions.y = dim[1];
	sd->dimensions.z = dim[2];

	sd->localPose.setColumnMajor44(mat);

	return sd;
}

#pragma comment(linker, "/export:PHShapeSphere@8=PHShapeSphere")
void* __stdcall PHShapeSphere(float radius, float *offset)
{
	NxSphereShapeDesc *sd = new NxSphereShapeDesc;
	sd->radius = radius;

	if(offset)
	{
		sd->localPose.t.x = offset[0];
		sd->localPose.t.y = offset[1];
		sd->localPose.t.z = offset[2];
	}

	return sd;
}

#pragma comment(linker, "/export:PHShapeSphereM44@8=PHShapeSphereM44")
void* __stdcall PHShapeSphereM44(float radius, float *mat)
{
	NxSphereShapeDesc *sd = new NxSphereShapeDesc;
	sd->radius = radius;

	sd->localPose.setColumnMajor44(mat);

	return sd;
}

#define simillar(a,b) (fabs(a-b) < 0.001)

#pragma comment(linker, "/export:PHShapeTrimesh@12=PHShapeTrimesh")
void* __stdcall PHShapeTrimesh(void *mesh, float *scale, float *offset)
{
	NxTriangleMeshShapeDesc *sd = new NxTriangleMeshShapeDesc;
	sd->meshData = (NxTriangleMesh*)mesh;
	
	if(offset)
	{
		sd->localPose.t.x = offset[0];
		sd->localPose.t.y = offset[1];
		sd->localPose.t.z = offset[2];
	}
	
	if(scale && !(simillar(scale[0],1.0f) && simillar(scale[1],1.0f) && simillar(scale[2],1.0f)))
		printf("Scale doesn't supported in PhysX 2\n");

	return sd;
}

#pragma comment(linker, "/export:PHShapeTrimeshM44@12=PHShapeTrimeshM44")
void* __stdcall PHShapeTrimeshM44(void *mesh, float *scale, float *mat)
{
	NxTriangleMeshShapeDesc *sd = new NxTriangleMeshShapeDesc;
	sd->meshData = (NxTriangleMesh*)mesh;
	
	sd->localPose.setColumnMajor44(mat);
	
	if(scale && !(simillar(scale[0],1.0f) && simillar(scale[1],1.0f) && simillar(scale[2],1.0f)))
		printf("Scale doesn't supported in PhysX 2\n");

	return sd;
}

#pragma comment(linker, "/export:PHCreateActor@20=PHCreateActor")
NxActor * __stdcall PHCreateActor(NxScene *pScene, int isdynamic, int count, NxShapeDesc **shapes, float *mat)
{
	NxBodyDesc bd;
	bd.mass = 1.0;
	bd.massSpaceInertia.set(0, 1, 0);
	
	NxActorDesc ad;
	ad.body = isdynamic ? &bd : NULL;
	ad.shapes.resize(count);
	for(int i = 0; i < count; i++)
		ad.shapes[i] = shapes[i];
	if(mat)
		ad.globalPose.setColumnMajor44(mat);
	
	NxActor *actor = pScene->createActor(ad);
	
	for(int i = 0; i < count; i++)
		delete shapes[i];

	return actor;
}

#pragma comment(linker, "/export:PHRemoveActor@8=PHRemoveActor")
void __stdcall PHRemoveActor(NxScene *pScene, NxActor* actor)
{
	if(actor)
		pScene->releaseActor(*actor);
}

#pragma comment(linker, "/export:PHSetGlobalPoseM44@8=PHSetGlobalPoseM44")
void __stdcall PHSetGlobalPoseM44(NxActor *actor, float *m)
{
	if(actor)
	{
		NxMat34 m34;
		m34.setColumnMajor44(m);

		actor->setGlobalPose(m34);

		NxScene *pScene = &actor->getScene();
		pScene->simulate(0.0);
		pScene->fetchResults(NX_ALL_FINISHED, true);
	}
}

#pragma comment(linker, "/export:PHSetUserdata@8=PHSetUserdata")
void* __stdcall PHSetUserdata(NxActor *actor, void* userdata)
{
	if(!actor)
		return NULL;

	void *old_data = actor->userData;
	actor->userData = userdata;
	return old_data;
}

#pragma comment(linker, "/export:PHGetUserdata@4=PHGetUserdata")
void* __stdcall PHGetUserdata(NxActor *actor)
{
	if(actor)
		return actor->userData;
	else
		return NULL;
}

#pragma comment(linker, "/export:PHGetShapeCount@4=PHGetShapeCount")
int __stdcall PHGetShapeCount(NxActor *actor)
{
	if(actor)
		return actor->getNbShapes();
	else
		return 0;
}

#pragma comment(linker, "/export:PHGetShape@8=PHGetShape")
NxShape* __stdcall PHGetShape(NxActor *actor, int id)
{
	if(actor)
		return actor->getShapes()[id];
	else
		return NULL;
}

#pragma comment(linker, "/export:PHAddShape@8=PHAddShape")
NxShape * __stdcall PHAddShape(NxActor *actor, NxShapeDesc *desc)
{
	if(actor && desc)
	{
		NxShape *shape = actor->createShape(*desc);
		delete desc;
		return shape;
	}
	else
		return NULL;
}

#pragma comment(linker, "/export:PHRemoveShape@4=PHRemoveShape")
void __stdcall PHRemoveShape(NxShape *shape)
{
	if(shape)
		shape->getActor().releaseShape(*shape);
}

#pragma comment(linker, "/export:PHGetActor@4=PHGetActor")
NxActor * __stdcall PHGetActor(NxShape *shape)
{
	if(shape)
		return &shape->getActor();
	else
		return 0;
}

#pragma comment(linker, "/export:PHSetShapeUserdata@8=PHSetShapeUserdata")
void* __stdcall PHSetShapeUserdata(NxShape *shape, void* userdata)
{
	if(!shape)
		return NULL;

	void *old_data = shape->userData;
	shape->userData = userdata;
	return old_data;
}

#pragma comment(linker, "/export:PHGetShapeUserdata@4=PHGetShapeUserdata")
void* __stdcall PHGetShapeUserdata(NxShape *shape)
{
	if(shape)
		return shape->userData;
	else
		return NULL;
}

#pragma comment(linker, "/export:PHSetGroup@8=PHSetGroup")
void __stdcall PHSetGroup(NxShape *shape, unsigned group)
{
	shape->setGroup(group);
}

#pragma comment(linker, "/export:PHGetGroup@4=PHGetGroup")
NxU32 __stdcall PHGetGroup(NxShape *shape)
{
	return shape->getGroup();
}

#pragma comment(linker, "/export:PHIsBoxShape@4=PHIsBoxShape")
int __stdcall PHIsBoxShape(NxShape *shape)
{
	return shape->getType() == NX_SHAPE_BOX;
}

#pragma comment(linker, "/export:PHIsSphereShape@4=PHIsSphereShape")
int __stdcall PHIsSphereShape(NxShape *shape)
{
	return shape->getType() == NX_SHAPE_SPHERE;
}

#pragma comment(linker, "/export:PHIsTrimeshShape@4=PHIsTrimeshShape")
int __stdcall PHIsTrimeshShape(NxShape *shape)
{
	return shape->getType() == NX_SHAPE_MESH;
}

#pragma comment(linker, "/export:PHSetBoxDim@8=PHSetBoxDim")
void __stdcall PHSetBoxDim(NxShape *shape, float *dim)
{
	if(!PHIsBoxShape(shape))
		return;
		
	NxBoxShape* box = (NxBoxShape*)shape;
	box->setDimensions(NxVec3(dim[0], dim[1], dim[2]));
}

#pragma comment(linker, "/export:PHSetBoxDim@8=PHSetBoxDim")
void __stdcall PHSetSphereRadius(NxShape *shape, float radius)
{
	if(!PHIsSphereShape(shape))
		return;
		
	NxSphereShape* sphere = (NxSphereShape*)shape;
	sphere->setRadius(radius);
}

#pragma comment(linker, "/export:PHSetMeshScale@8=PHSetMeshScale")
void __stdcall PHSetMeshScale(NxShape *shape, float *scale)
{
	// not supported
}
