#include <stdio.h>
#include <vector>
#include <windows.h> // for MessageBox

#include "physx2\PhysXLoader.h"
#include "physx2\NxCooking.h"
#include "physx2\NxStream.h"
#include "physx2\NxUserOutputStream.h"
#include "physx2\NxTriangleMeshDesc.h"

#ifdef _M_AMD64
#pragma comment(lib, "PhysXLoader64.lib")
#else
#pragma comment(lib, "PhysXLoader.lib")
#endif

class MyNxStream : public NxStream
{
	public:

	virtual NxU8 readByte() const;
	virtual NxU16 readWord() const;
	virtual NxU32 readDword() const;
	virtual NxF32 readFloat() const;
	virtual NxF64 readDouble() const;
	virtual void readBuffer(void *buffer, NxU32 size) const;
	
	virtual NxStream& storeByte(NxU8 n);
	virtual NxStream& storeWord(NxU16 n);
	virtual NxStream& storeDword(NxU32 n);
	virtual NxStream& storeFloat(NxF32 n);
	virtual NxStream& storeDouble(NxF64 n);
	virtual NxStream& storeBuffer(const void *buffer, NxU32 size);	
};

NxU8 MyNxStream::readByte() const
{
	NxU8 ret;
	readBuffer(&ret, sizeof(ret));
	return ret;
}

NxU16 MyNxStream::readWord() const
{
	NxU16 ret;
	readBuffer(&ret, sizeof(ret));
	return ret;
}

NxU32 MyNxStream::readDword() const
{
	NxU32 ret;
	readBuffer(&ret, sizeof(ret));
	return ret;
}

NxF32 MyNxStream::readFloat() const
{
	NxF32 ret;
	readBuffer(&ret, sizeof(ret));
	return ret;
}

NxF64 MyNxStream::readDouble() const
{
	NxF64 ret;
	readBuffer(&ret, sizeof(ret));
	return ret;
}

void MyNxStream::readBuffer(void *buffer, NxU32 size) const
{
	printf("pure virtual function call (MyNxStream::readBuffer)\n");
}

NxStream& MyNxStream::storeByte(NxU8 n)
{
	storeBuffer(&n, sizeof(n));
	return *this;
}

NxStream& MyNxStream::storeWord(NxU16 n)
{
	storeBuffer(&n, sizeof(n));
	return *this;
}

NxStream& MyNxStream::storeDword(NxU32 n)
{
	storeBuffer(&n, sizeof(n));
	return *this;
}

NxStream& MyNxStream::storeFloat(NxF32 n)
{
	storeBuffer(&n, sizeof(n));
	return *this;
}

NxStream& MyNxStream::storeDouble(NxF64 n)
{
	storeBuffer(&n, sizeof(n));
	return *this;
}

NxStream& MyNxStream::storeBuffer(const void *buffer, NxU32 size)
{
	printf("pure virtual function call (MyNxStream::storeBuffer)\n");
	return *this;
}

class MyNxMemReader
	: public MyNxStream
{
	public:
	void *data;
	size_t datasize;
	mutable size_t ptr;
	
	MyNxMemReader(void *src, size_t srcsize);
	~MyNxMemReader();
	
	virtual void readBuffer(void *buffer, NxU32 size) const;
};

MyNxMemReader::MyNxMemReader(void *src, size_t srcsize)
	: data(src),
	  datasize(srcsize),
	  ptr(0)
{
}

MyNxMemReader::~MyNxMemReader()
{
}

void MyNxMemReader::readBuffer(void *buffer, NxU32 size) const
{
	size_t nread = datasize-ptr;
	if(size < nread)
		nread = size;
	memcpy(buffer, (char*)data + ptr, nread);
	ptr += nread;
}

class MyNxMemWriter
	: public MyNxStream
{
	public:
	std::vector<char> data;
	
	void clear();
	virtual NxStream& storeBuffer(const void *buffer, NxU32 size);
};

void MyNxMemWriter::clear()
{
	data.clear();
}

NxStream& MyNxMemWriter::storeBuffer(const void *buffer, NxU32 size)
{
	size_t pos = data.size();
	data.resize(data.size() + size);
	memcpy(&data[pos], buffer, size);
	return *this;
}

class MyNxErrorStream
	: public NxUserOutputStream
{
	public:
	virtual void reportError(NxErrorCode code, const char * message, const char *file, int line);
	virtual NxAssertResponse reportAssertViolation(const char * message, const char *file, int line);
	virtual void print(const char * message);
};

void MyNxErrorStream::reportError(NxErrorCode code, const char * message, const char * file, int line)
{
	printf("[PhysX] ERROR %s:%d %s\n", file, line, message);
}

NxAssertResponse MyNxErrorStream::reportAssertViolation(const char * message, const char * file, int line)
{
	printf("[PhysX] FATAL ERROR %s:%d %s\n", file, line, message);
	int ret = MessageBox(NULL, message, NULL, MB_ABORTRETRYIGNORE|MB_ICONERROR);
	
	switch(ret)
	{
		case IDABORT:			return NX_AR_BREAKPOINT;
		case IDRETRY:			return NX_AR_IGNORE;
		case IDCONTINUE:	return NX_AR_CONTINUE;
	}
	
	printf("???\n");
	return NX_AR_IGNORE;
}

void MyNxErrorStream::print(const char * message)
{
	printf("[PhysX] %s\n", message);
}

static unsigned nxcooking_initcounter = 0;
static NxCookingInterface *nxcooking;

#pragma comment(linker, "/export:InitCooking@0=InitCooking")
int __stdcall InitCooking(void)
{
	static MyNxErrorStream errorStream;
	
	nxcooking = NxGetCookingLib(NX_PHYSICS_SDK_VERSION);
	
	if(nxcooking && nxcooking->NxInitCooking(NULL, &errorStream))
	{
		nxcooking_initcounter++;
		return 1;
	}
	else
		return 0;
}

#pragma comment(linker, "/export:CloseCooking@0=CloseCooking")
int __stdcall CloseCooking(void)
{
	if(nxcooking)
	{
		nxcooking->NxCloseCooking();
		nxcooking_initcounter--;
		
		if(nxcooking_initcounter == 0)
			nxcooking = NULL;
			
		return 1;
	}
	else
		return 0;
}

#pragma comment(linker, "/export:SetCookingParams@12=SetCookingParams")
int __stdcall SetCookingParams(int platform, float skinWidth, int hintCollisionSpeed)
{
	NxCookingParams params;

	params.targetPlatform = (NxPlatform)platform;
	params.skinWidth = skinWidth;
	params.hintCollisionSpeed = hintCollisionSpeed;

	if(nxcooking->NxSetCookingParams(params))
		return 1;
	else
		return 0;
}

#pragma comment(linker, "/export:GetCookingParams@12=GetCookingParams")
void __stdcall GetCookingParams(int *platform, float *skinWidth, int *hintCollisionSpeed)
{
	NxCookingParams params = nxcooking->NxGetCookingParams();

	*platform = params.targetPlatform;
	*skinWidth = params.skinWidth;
	*hintCollisionSpeed = params.hintCollisionSpeed;
}

#pragma comment(linker, "/export:CookTriangleMesh@36=CookTriangleMesh")
int __stdcall CookTriangleMesh
(int numVertices, int numTriangles, void *ppoints, void *ptris,
	int pointstride, int trianglestride, int flags, void **ptr, int *sz)
{
	if(!nxcooking)
		return 0;
				
	NxTriangleMeshDesc desc;
	desc.numVertices = numVertices;;
	desc.numTriangles = numTriangles;
	desc.pointStrideBytes = pointstride;
	desc.triangleStrideBytes = trianglestride;
	desc.points = ppoints;
	desc.triangles = ptris;
	desc.flags = flags;	
		
	if(desc.isValid())
	{
		static MyNxMemWriter mem;

		mem.clear();
		bool ret = nxcooking->NxCookTriangleMesh(desc, mem);
		//if(ret)
		//	printf("NxCookTriangleMesh ok\n");

		if(ret)
		{
			*ptr = &mem.data[0];
			*sz = (int)mem.data.size();
			
			return 1;
		}
	}
	
	return 0;
}