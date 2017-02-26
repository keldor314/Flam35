__shared__ unsigned int randStates[32];

__constant__ unsigned int cshift1[4] = { 6, 2, 13, 3 };
__constant__ unsigned int cshift2[4] = { 13, 27, 21, 12 };
__constant__ unsigned int cshift3[4] = { 18, 2, 7, 13 };
__constant__ unsigned int coffset[4] = { 4294967294, 4294967288, 4294967280, 4294967168 };

__shared__ unsigned int shift1[4];
__shared__ unsigned int shift2[4];
__shared__ unsigned int shift3[4];
__shared__ unsigned int offset[4];

__device__ unsigned int TausStep(unsigned int &z)
{
	int index = threadIdx.x & 3;
	unsigned int b = (((z << shift1[index]) ^ z) >> shift2[index]);
	return z = (((z & offset[index]) << shift3[index]) ^ b);
}

__device__ unsigned int randInt()
{
	TausStep(randStates[threadIdx.x & 31]);
	return (randStates[(threadIdx.x) & 31] ^ randStates[(threadIdx.x + 1) & 31] ^ randStates[(threadIdx.x + 2) & 31] ^ randStates[(threadIdx.x + 3) & 31]);
}

extern "C" __global__ void AtomicPointPoolTest(float4* pointPool, unsigned int* irandStates)
{
	if (threadIdx.x < 4)
	{
		shift1[threadIdx.x] = cshift1[threadIdx.x];
		shift2[threadIdx.x] = cshift2[threadIdx.x];
		shift3[threadIdx.x] = cshift3[threadIdx.x];
		offset[threadIdx.x] = coffset[threadIdx.x];
	}
	randStates[threadIdx.x] = irandStates[threadIdx.x + 32 * blockIdx.x];
	for (int n = 0; n < 256; n++)
	{
		unsigned int input = randInt() & 255;
		unsigned int output = randInt() & 255;
		pointPool[output] = pointPool[input];
	}
}
