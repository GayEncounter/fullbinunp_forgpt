#include <windows.h>
#include <stdio.h>
#include <malloc.h>
#include <vector>

#include "physx3\PxPhysicsAPI.h"

#ifdef _WIN64
#define DLL_SUFFIX "_x64"
#else
#define DLL_SUFFIX "_x86"
#endif

#define printptr(v) printf(#v " = 0x%p\n", v)

using namespace physx;

// типы функций, дабы не лепить .lib Файлы
typedef PxFoundation *(PX_CALL_CONV *LPFNPXCREATEFOUNDATIONPROC)(
	PxU32 version,
	PxAllocatorCallback &allocator,
	PxErrorCallback &errorCallback);

typedef PxPhysics *(PX_CALL_CONV *LPFNPXCREATEBASEPHYSICSPROC)(
	PxU32 version, 
	PxFoundation &foundation, 
	PxTolerancesScale &scale, 
	bool trackOutstandingAllocations,
	PxProfileZoneManager *profileZoneManager);
	
typedef PxCooking *(PX_CALL_CONV *LPFNPXCREATECOOKINGPROC)(
	PxU32 version,
	PxFoundation &foundation,
	const PxCookingParams &params);
	
// реализация говноинтерфейсов
class MyPxAllocator
	: public PxAllocatorCallback
{
	public:
	virtual void * allocate(size_t size, const char *typeName, const char *filename, int line);
	virtual void deallocate(void *ptr);
};

void * MyPxAllocator::allocate(size_t size, const char *typeName, const char *filename, int line)
{
	void *ret = _aligned_malloc(size, 16);
	return ret;
}

void MyPxAllocator::deallocate(void *ptr)
{
	_aligned_free(ptr);
}

class MyPxError
	: public PxErrorCallback
{
	public:
	virtual void reportError(PxErrorCode::Enum code, const char *message, const char *file, int line);
};

void MyPxError::reportError(PxErrorCode::Enum code, const char *message, const char *file, int line)
{
	printf("[PhysX3] %s:%d %s\n", file, line, message);
}

class MyPxMemReader
	: public PxInputStream
{
	public:
	void *buffer;
	size_t bufsize;
	size_t pointer;
	
	MyPxMemReader(void *source, size_t source_size);
	virtual ~MyPxMemReader();
	
	virtual PxU32 read(void *dest, PxU32 count);
};

MyPxMemReader::MyPxMemReader(void *source, size_t source_size)
	: buffer(source),
	  bufsize(source_size),
	  pointer(0)
{
}

MyPxMemReader::~MyPxMemReader()
{
}

PxU32 MyPxMemReader::read(void *dest, PxU32 count)
{
	size_t nread = bufsize-pointer;
	if(!(count > nread))
		nread = count;
		
	memcpy(dest, (char*)buffer + pointer, nread);
	pointer += nread;
	
	return nread;
}

class MyPxMemWriter
	: public PxOutputStream
{
	public:
	std::vector<char> data;
	
	MyPxMemWriter();
	virtual ~MyPxMemWriter();
	
	void clear();
	virtual PxU32 write(const void *src, PxU32 count); 
};

MyPxMemWriter::MyPxMemWriter()
{
}

MyPxMemWriter::~MyPxMemWriter()
{
}

void MyPxMemWriter::clear()
{
	data.clear();
}

PxU32 MyPxMemWriter::write(const void *src, PxU32 count)
{
	size_t ptr = data.size();
	data.resize(data.size() + count);
	memcpy(&data[ptr], src, count);
	return count;
}

class MyPxQueryFilterGroup
	: public PxQueryFilterCallback
{
	public:
	PxU32 groups;
	
	MyPxQueryFilterGroup(PxU32 allowed_groups)
		: groups(allowed_groups)
	{
	}
	
	virtual PxQueryHitType::Enum preFilter(const PxFilterData &filterData, const PxShape *shape, const PxRigidActor *actor, PxHitFlags &queryFlags);
	virtual PxQueryHitType::Enum postFilter(const PxFilterData &filterData, const PxQueryHit &hit);
};

PxQueryHitType::Enum MyPxQueryFilterGroup::preFilter(const PxFilterData &filterData, const PxShape *shape, const PxRigidActor *actor, PxHitFlags &queryFlags)
{
	PxU32 sgrp = shape->getQueryFilterData().word0;
	if((1<<sgrp) & groups)
		return PxQueryHitType::eBLOCK;
	else
		return PxQueryHitType::eNONE;
}

PxQueryHitType::Enum MyPxQueryFilterGroup::postFilter(const PxFilterData &filterData, const PxQueryHit &hit)
{
	PxU32 sgrp = hit.shape->getQueryFilterData().word0;
	if((1<<sgrp) & groups)
		return PxQueryHitType::eBLOCK;
	else
		return PxQueryHitType::eNONE;
}

// глобальные переменные
UINT init_cooking3_counter = 0;
BOOL initialized = FALSE;

HMODULE hPhysX3;
HMODULE hPhysX3Common;
HMODULE hPhysX3Cooking;

PxFoundation *foundation;
PxPhysics *physics;
PxCooking *cooking;

MyPxAllocator allocator;
MyPxError error;
PxTolerancesScale tolerancesScale;

PxMaterial *default_material;

#pragma comment(linker, "/export:InitCooking3@0=InitCooking3")
int __stdcall InitCooking3(void)
{
	if(init_cooking3_counter == 0)
	{
		//
		hPhysX3Common = LoadLibrary("PhysX3Common" DLL_SUFFIX ".dll");
		printptr(hPhysX3Common);
		
		hPhysX3Cooking = LoadLibrary("PhysX3Cooking" DLL_SUFFIX ".dll");
		printptr(hPhysX3Cooking);
		
		//
		FARPROC p1 = GetProcAddress(hPhysX3Common, "PxCreateFoundation");
		printptr(p1);
		
		FARPROC p2 = GetProcAddress(hPhysX3Cooking, "PxCreateCooking");
		printptr(p2);
		
		//
		LPFNPXCREATEFOUNDATIONPROC PxCreateFoundation = (LPFNPXCREATEFOUNDATIONPROC)p1;
		LPFNPXCREATECOOKINGPROC PxCreateCooking = (LPFNPXCREATECOOKINGPROC)p2;
		
		allocator = MyPxAllocator();
		error = MyPxError();
		
		foundation = PxCreateFoundation(PX_PHYSICS_VERSION, allocator, error);
		printptr(foundation);
		
		cooking = PxCreateCooking(PX_PHYSICS_VERSION, *foundation, PxCookingParams(tolerancesScale));
		printptr(cooking);
	}
	
	init_cooking3_counter++;

	return 1;
}

#pragma comment(linker, "/export:CloseCooking3@0=CloseCooking3")
void __stdcall CloseCooking3(void)
{
	if(init_cooking3_counter > 0)
		init_cooking3_counter--;
		
	if(init_cooking3_counter == 0)
	{
		//
		cooking->release();
		foundation->release();
		
		//
		FreeLibrary(hPhysX3Common);
		FreeLibrary(hPhysX3Cooking);
			
		//
		cooking = NULL;
		foundation = NULL;
		
		hPhysX3Common = NULL;
		hPhysX3Cooking = NULL;
	}
}

#pragma comment(linker, "/export:CookTriangleMesh3@36=CookTriangleMesh3")
int __stdcall CookTriangleMesh3(int numVerts, int numTriangles, void *pVerts,
	void *pTris, int vertStride, int triangleStride, int flags, void **ptr, int *sz)
{
	if(init_cooking3_counter == 0)
		return 0;
	
	//	
	PxTriangleMeshDesc desc;
	
	desc.points.stride = vertStride;
	desc.points.data = pVerts;
	desc.points.count = numVerts;
	
	desc.triangles.stride = triangleStride;
	desc.triangles.data = pTris;
	desc.triangles.count = numTriangles;
	
	desc.flags = PxMeshFlags(flags & 3); // hack - exclude NX_MF_HARDWARE_MESH
	
	if(!desc.isValid())
		return 0;
	
	//
	static MyPxMemWriter out;
	out.clear();
	
	if(!cooking->cookTriangleMesh(desc, out))
		return 0;
	
	*ptr = &out.data[0];
	*sz = (int)out.data.size();
			
	return 1;
}

#pragma comment(linker, "/export:PHInitialize@0=PHInitialize")
int __stdcall PHInitialize(void)
{
	if(!initialized)
	{
		if(!InitCooking3())
			return 0;
			
		//
		hPhysX3 = LoadLibrary("PhysX3" DLL_SUFFIX ".dll");
		printptr(hPhysX3);
		
		//
		FARPROC p1 = GetProcAddress(hPhysX3, "PxCreateBasePhysics");
		printptr(p1);
		
		//
		LPFNPXCREATEBASEPHYSICSPROC PxCreateBasePhysics = (LPFNPXCREATEBASEPHYSICSPROC)p1;
		
		tolerancesScale = PxTolerancesScale();
		physics = PxCreateBasePhysics(PX_PHYSICS_VERSION, *foundation, tolerancesScale, false, NULL);
		printptr(physics);
		
		default_material = physics->createMaterial(1.0, 1.0, 0.0);
		printptr(default_material);
		
		initialized = TRUE;
	}

	return 1;
}

#pragma comment(linker, "/export:PHFinalize@0=PHFinalize")
void __stdcall PHFinalize(void)
{
	if(initialized)
	{
		default_material->release();
	
		//
		physics->release();
		
		//
		FreeLibrary(hPhysX3);
			
		//
		default_material = NULL;
		physics = NULL;
		hPhysX3 = NULL;
		
		//
		CloseCooking3();
		
		initialized = FALSE;
	}
}

#pragma comment(linker, "/export:PHCreateTriMesh@28=PHCreateTriMesh")
PxTriangleMesh * __stdcall PHCreateTriMesh(int numVerts, int numTriangles, void *pVerts,
	void *pTris, int vertStride, int triangleStride, int flags)
{
	if(!initialized)
		return NULL;
	
	//	
	PxTriangleMeshDesc desc;
	
	desc.points.stride = vertStride;
	desc.points.data = pVerts;
	desc.points.count = numVerts;
	
	desc.triangles.stride = triangleStride;
	desc.triangles.data = pTris;
	desc.triangles.count = numTriangles;
	
	desc.flags = PxMeshFlags(flags & 3); // hack - exclude NX_MF_HARDWARE_MESH
	
	if(!desc.isValid())
		return NULL;
	
	//
	MyPxMemWriter out;
	if(!cooking->cookTriangleMesh(desc, out))
		return NULL;
		
	//
	MyPxMemReader in(&out.data[0], out.data.size());
	return physics->createTriangleMesh(in);
}

#pragma comment(linker, "/export:PHLoadTriMesh@8=PHLoadTriMesh")
PxTriangleMesh * __stdcall PHLoadTriMesh(void *data, int size)
{
	MyPxMemReader st(data, size);
	return physics->createTriangleMesh(st);
}

#pragma comment(linker, "/export:PHFreeTriMesh@4=PHFreeTriMesh")
void __stdcall PHFreeTriMesh(PxTriangleMesh *mesh)
{
	if(mesh)
		mesh->release();
}

#pragma comment(linker, "/export:PHCreateScene@0=PHCreateScene")
PxScene * __stdcall PHCreateScene(void)
{
	if(!physics)
		return NULL;
		
	return physics->createScene(PxSceneDesc(tolerancesScale));
}

#pragma comment(linker, "/export:PHDestroyScene@4=PHDestroyScene")
void  __stdcall PHDestroyScene(PxScene *scene)
{
	if(scene)
		scene->release();
}

#pragma comment(linker, "/export:PHCullShapes@24=PHCullShapes")
int __stdcall PHCullShapes(PxScene *pScene, int num_planes, float *planes, int num_shapes, PxShape **shapes, unsigned group)
{
	// not implemented !
	return 0;
}

#pragma comment(linker, "/export:PHRaycastAnyShape@20=PHRaycastAnyShape")
int __stdcall PHRaycastAnyShape(PxScene *scene, float *p, float *dir, float dist, unsigned group)
{
	PxVec3 origin(p[0], p[1], p[2]); 
	PxVec3 direction(dir[0], dir[1], dir[2]);
	
	PxRaycastBufferN<1> result;
	
	PxHitFlags hit_flags(PxHitFlag::eMESH_ANY);
	PxQueryFilterData filter_data(PxQueryFlag::eSTATIC|PxQueryFlag::eDYNAMIC|PxQueryFlag::eANY_HIT);
	
	if(group != 0xFFFFFFFF)
	{
		filter_data.flags |= PxQueryFlag::ePREFILTER|PxQueryFlag::ePOSTFILTER;
		
		MyPxQueryFilterGroup filter(group);
		if(scene->raycast(origin, direction, dist, result, hit_flags, filter_data, &filter))
			return 1;
	}
	else
	{
		if(scene->raycast(origin, direction, dist, result, hit_flags, filter_data))
			return 1;
	}

	return 0;
}

#pragma comment(linker, "/export:PHRaycastClosestShape@32=PHRaycastClosestShape")
PxShape * __stdcall PHRaycastClosestShape(PxScene *scene, float *p, float *dir, float dist, float *hit_pos, float *hit_nrm, float *uv, unsigned group)
{
	PxVec3 origin(p[0], p[1], p[2]); 
	PxVec3 direction(dir[0], dir[1], dir[2]);
	
	PxRaycastBufferN<1> result;
	
	PxHitFlags hit_flags;
	PxQueryFilterData filter_data(PxQueryFlag::eSTATIC|PxQueryFlag::eDYNAMIC);
	
	bool hit;
	
	if(hit_pos)
		hit_flags |= PxHitFlag::ePOSITION;
	if(hit_nrm)
		hit_flags |= PxHitFlag::eNORMAL;
	if(uv)
		hit_flags |= PxHitFlag::eUV;
	
	if(group != 0xFFFFFFFF)
	{
		filter_data.flags |= PxQueryFlag::ePREFILTER|PxQueryFlag::ePOSTFILTER;
		
		MyPxQueryFilterGroup filter(group);
		hit = scene->raycast(origin, direction, dist, result, hit_flags, filter_data, &filter);
	}
	else
	{
		//printf("actors %d\n", scene->getNbActors(PxActorTypeFlag::eRIGID_STATIC|PxActorTypeFlag::eRIGID_DYNAMIC));
		//scene->forceDynamicTreeRebuild(true, true);
		hit = scene->raycast(origin, direction, dist, result, hit_flags, filter_data);
	}
	
	if(hit)
	{
		if(hit_pos)
		{
			hit_pos[0] = result.hits[0].position.x;
			hit_pos[1] = result.hits[0].position.y;
			hit_pos[2] = result.hits[0].position.z;
		}

		if(hit_nrm)
		{
			hit_nrm[0] = result.hits[0].normal.x;
			hit_nrm[1] = result.hits[0].normal.y;
			hit_nrm[2] = result.hits[0].normal.z;
		}

		if(uv)
		{
			uv[0] = result.hits[0].u;
			uv[1] = result.hits[1].v;
		}
		
		return result.hits[0].shape;
	}
	else
		return NULL;
}

#pragma comment(linker, "/export:PHRaycastClosestActor@32=PHRaycastClosestActor")
PxActor * __stdcall PHRaycastClosestActor(PxScene *scene, float *p, float *dir, float dist, float *hit_pos, float *hit_nrm, float *uv, unsigned group)
{
	PxShape *result = PHRaycastClosestShape(scene, p, dir, dist, hit_pos, hit_nrm, uv, group);
	if(result)
		return result->getActor();
	else
		return NULL;
}

#pragma comment(linker, "/export:PHShapeBox@8=PHShapeBox")
void* __stdcall PHShapeBox(float *dim, float *offset)
{
	PxBoxGeometry geom(dim[0], dim[1], dim[2]);

	PxShape *shape = physics->createShape(geom, *default_material, true);
	if(shape)
	{
		if(offset)
		{
			PxTransform pose = PxTransform(PxVec3(offset[0], offset[1], offset[2]));
			shape->setLocalPose(pose);
		}
		
		return shape;
	}
	
	return NULL;
}

#pragma comment(linker, "/export:PHShapeBoxM44@8=PHShapeBoxM44")
void* __stdcall PHShapeBoxM44(float *dim, float *mat)
{
	PxBoxGeometry geom(dim[0], dim[1], dim[2]);

	PxShape *shape = physics->createShape(geom, *default_material, true);
	if(shape)
	{
		if(mat)
		{
			PxTransform pose = PxTransform(PxMat44(mat));
			shape->setLocalPose(pose);
		}
		
		return shape;
	}
	
	return NULL;
}

#pragma comment(linker, "/export:PHShapeSphere@8=PHShapeSphere")
void* __stdcall PHShapeSphere(float radius, float *offset)
{
	PxSphereGeometry geom(radius);

	PxShape *shape = physics->createShape(geom, *default_material, true);
	if(shape)
	{
		if(offset)
		{
			PxTransform pose = PxTransform(PxVec3(offset[0], offset[1], offset[2]));
			shape->setLocalPose(pose);
		}
		
		return shape;
	}
	
	return NULL;
}

#pragma comment(linker, "/export:PHShapeSphereM44@8=PHShapeSphereM44")
void* __stdcall PHShapeSphereM44(float radius, float *mat)
{
	PxSphereGeometry geom(radius);

	PxShape *shape = physics->createShape(geom, *default_material, true);
	if(shape)
	{
		if(mat)
		{
			PxTransform pose = PxTransform(PxMat44(mat));
			shape->setLocalPose(pose);
		}
		
		return shape;
	}
	
	return NULL;
}

#pragma comment(linker, "/export:PHShapeTrimesh@12=PHShapeTrimesh")
void* __stdcall PHShapeTrimesh(PxTriangleMesh *mesh, float *scale, float *offset)
{
	PxTriangleMeshGeometry geom;
	geom.triangleMesh = mesh;
	
	if(scale)
	{
		geom.scale.scale.x = scale[0];
		geom.scale.scale.y = scale[1];
		geom.scale.scale.z = scale[2];
	}
	
	PxShape *shape = physics->createShape(geom, *default_material, true);
	if(shape)
	{
		if(offset)
		{
			PxTransform pose(PxVec3(offset[0], offset[1], offset[2]));
			shape->setLocalPose(pose);
		}
		
		return shape;
	}
	
	return NULL;
}

#pragma comment(linker, "/export:PHCreateActor@20=PHCreateActor")
PxRigidActor * __stdcall PHCreateActor(PxScene *scene, int isdynamic, int count, PxShape **shapes, float *mat)
{
	PxTransform pose;
	
	if(mat)
		pose = PxTransform(PxMat44(mat));
	else
		pose = PxTransform(PxIdentity);
		
	PxRigidActor *actor;
	
	if(isdynamic)
		actor = physics->createRigidDynamic(pose);
	else
		actor = physics->createRigidStatic(pose);
	
	if(!actor)
		return NULL;
		
	for(int i = 0; i < count; i++)
	{
		actor->attachShape(*shapes[i]);
		shapes[i]->release();
	}
	
	//printf("shapes %d\n", actor->getNbShapes());
		
	scene->addActor(*actor);
	return actor;
}

#pragma comment(linker, "/export:PHRemoveActor@8=PHRemoveActor")
void __stdcall PHRemoveActor(PxScene *scene, PxRigidActor* actor)
{
	if(actor)
	{
		scene->removeActor(*actor);
		actor->release();
	}
}

#pragma comment(linker, "/export:PHSetGlobalPoseM44@8=PHSetGlobalPoseM44")
void __stdcall PHSetGlobalPoseM44(PxRigidActor *actor, float *m)
{
	if(actor)
	{
		PxTransform pose = PxTransform(PxMat44(m));
		actor->setGlobalPose(pose);
	}
}

#pragma comment(linker, "/export:PHSetUserdata@8=PHSetUserdata")
void* __stdcall PHSetUserdata(PxRigidActor *actor, void* userdata)
{
	if(!actor)
		return NULL;

	void *old_data = actor->userData;
	actor->userData = userdata;
	return old_data;
}

#pragma comment(linker, "/export:PHGetUserdata@4=PHGetUserdata")
void* __stdcall PHGetUserdata(PxRigidActor *actor)
{
	if(actor)
		return actor->userData;
	else
		return NULL;
}

#pragma comment(linker, "/export:PHGetShapeCount@4=PHGetShapeCount")
int __stdcall PHGetShapeCount(PxRigidActor *actor)
{
	if(actor)
		return actor->getNbShapes();
	else
		return 0;
}

#pragma comment(linker, "/export:PHGetShape@8=PHGetShape")
PxShape* __stdcall PHGetShape(PxRigidActor *actor, int id)
{
	if(actor)
	{
		PxShape *shape;
		actor->getShapes(&shape, 1, id);
		/// should we call shape->release() here ?
		return shape;
	}
	else
		return NULL;
}

#pragma comment(linker, "/export:PHAddShape@8=PHAddShape")
PxShape * __stdcall PHAddShape(PxRigidActor *actor, PxShape *shape)
{
	if(actor && shape)
	{
		actor->attachShape(*shape);
		shape->release();
		return shape;
	}
	else
		return NULL;
}

#pragma comment(linker, "/export:PHRemoveShape@4=PHRemoveShape")
void __stdcall PHRemoveShape(PxShape *shape)
{
	if(shape)
	{
		shape->getActor()->detachShape(*shape);
	}
}

#pragma comment(linker, "/export:PHGetActor@4=PHGetActor")
PxRigidActor * __stdcall PHGetActor(PxShape *shape)
{
	if(shape)
		return shape->getActor();
	else
		return 0;
}

#pragma comment(linker, "/export:PHSetShapeUserdata@8=PHSetShapeUserdata")
void* __stdcall PHSetShapeUserdata(PxShape *shape, void* userdata)
{
	if(!shape)
		return NULL;

	void *old_data = shape->userData;
	shape->userData = userdata;
	return old_data;
}

#pragma comment(linker, "/export:PHGetShapeUserdata@4=PHGetShapeUserdata")
void* __stdcall PHGetShapeUserdata(PxShape *shape)
{
	if(shape)
		return shape->userData;
	else
		return NULL;
}

#pragma comment(linker, "/export:PHSetGroup@8=PHSetGroup")
void __stdcall PHSetGroup(PxShape *shape, unsigned group)
{
	PxFilterData data;
	data.word0 = group;
	shape->setQueryFilterData(data);
}

#pragma comment(linker, "/export:PHGetGroup@4=PHGetGroup")
unsigned __stdcall PHGetGroup(PxShape *shape)
{
	return shape->getQueryFilterData().word0;
}

#pragma comment(linker, "/export:PHIsBoxShape@4=PHIsBoxShape")
int __stdcall PHIsBoxShape(PxShape *shape)
{
	return shape->getGeometryType() == PxGeometryType::eBOX;
}

#pragma comment(linker, "/export:PHIsSphereShape@4=PHIsSphereShape")
int __stdcall PHIsSphereShape(PxShape *shape)
{
	return shape->getGeometryType() == PxGeometryType::eSPHERE;
}

#pragma comment(linker, "/export:PHIsTrimeshShape@4=PHIsTrimeshShape")
int __stdcall PHIsTrimeshShape(PxShape *shape)
{
	return shape->getGeometryType() == PxGeometryType::eTRIANGLEMESH;
}

#pragma comment(linker, "/export:PHSetBoxDim@8=PHSetBoxDim")
void __stdcall PHSetBoxDim(PxShape *shape, float *dim)
{
	if(!PHIsBoxShape(shape))
		return;
		
	PxBoxGeometry geom;
	shape->getBoxGeometry(geom);
	
	geom.halfExtents.x = dim[0];
	geom.halfExtents.y = dim[1];
	geom.halfExtents.z = dim[2];
	
	shape->setGeometry(geom);
}

#pragma comment(linker, "/export:PHSetSphereRadius@8=PHSetSphereRadius")
void __stdcall PHSetSphereRadius(PxShape *shape, float radius)
{
	if(!PHIsSphereShape(shape))
		return;
		
	PxSphereGeometry geom;
	shape->getSphereGeometry(geom);
	
	geom.radius = radius;
	
	shape->setGeometry(geom);
}

#pragma comment(linker, "/export:PHSetMeshScale@8=PHSetMeshScale")
void __stdcall PHSetMeshScale(PxShape *shape, float *scale)
{
	if(!PHIsTrimeshShape(shape))
		return;
		
	PxTriangleMeshGeometry geom;
	shape->getTriangleMeshGeometry(geom);
	
	geom.scale.scale.x = scale[0];
	geom.scale.scale.y = scale[1];
	geom.scale.scale.z = scale[2];
	
	shape->setGeometry(geom);
}