{-# LANGUAGE TemplateHaskell #-}

module Control.Parallel.OpenCL.Types
  ( module Control.Parallel.OpenCL.Types,
  )
where

import Bindings.OpenCL.Constants
import Bindings.OpenCL.Types
import Foreign.C.Convertable
import Foreign.C.Convertable.TH

convertableType
  [ ("Error'None", "c'CL_SUCCESS"),
    ("Error'DeviceNotFound", "c'CL_DEVICE_NOT_FOUND"),
    ("Error'DeviceNotAvailable", "c'CL_DEVICE_NOT_AVAILABLE"),
    ("Error'CompilerNotAvailable", "c'CL_COMPILER_NOT_AVAILABLE"),
    ("Error'MemObjectAllocationFailure", "c'CL_MEM_OBJECT_ALLOCATION_FAILURE"),
    ("Error'OutOfResources", "c'CL_OUT_OF_RESOURCES"),
    ("Error'OutOfHostMemory", "c'CL_OUT_OF_HOST_MEMORY"),
    ("Error'ProfilingInfoNotAvailable", "c'CL_PROFILING_INFO_NOT_AVAILABLE"),
    ("Error'MemCopyOverlap", "c'CL_MEM_COPY_OVERLAP"),
    ("Error'ImageFormatMismatch", "c'CL_IMAGE_FORMAT_MISMATCH"),
    ("Error'ImageFormatNotSupported", "c'CL_IMAGE_FORMAT_NOT_SUPPORTED"),
    ("Error'BuildProgramFailure", "c'CL_BUILD_PROGRAM_FAILURE"),
    ("Error'MapFailure", "c'CL_MAP_FAILURE"),
    ("Error'MisalignedSubBufferOffset", "c'CL_MISALIGNED_SUB_BUFFER_OFFSET"),
    ("Error'ExecStatusErrorForEventsInWaitList", "c'CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST"),
    ("Error'CompileProgramFailure", "c'CL_COMPILE_PROGRAM_FAILURE"),
    ("Error'LinkerNotAvailable", "c'CL_LINKER_NOT_AVAILABLE"),
    ("Error'LinkProgramFailure", "c'CL_LINK_PROGRAM_FAILURE"),
    ("Error'DevicePartitionFailed", "c'CL_DEVICE_PARTITION_FAILED"),
    ("Error'KernelArgInfoNotAvailable", "c'CL_KERNEL_ARG_INFO_NOT_AVAILABLE"),
    ("Error'InvalidValue", "c'CL_INVALID_VALUE"),
    ("Error'InvalidDeviceType", "c'CL_INVALID_DEVICE_TYPE"),
    ("Error'InvalidPlatform", "c'CL_INVALID_PLATFORM"),
    ("Error'InvalidDevice", "c'CL_INVALID_DEVICE"),
    ("Error'InvalidContext", "c'CL_INVALID_CONTEXT"),
    ("Error'InvalidQueueProperties", "c'CL_INVALID_QUEUE_PROPERTIES"),
    ("Error'InvalidCommandQueue", "c'CL_INVALID_COMMAND_QUEUE"),
    ("Error'InvalidHostPtr", "c'CL_INVALID_HOST_PTR"),
    ("Error'InvalidMemObject", "c'CL_INVALID_MEM_OBJECT"),
    ("Error'InvalidImageFormatDescriptor", "c'CL_INVALID_IMAGE_FORMAT_DESCRIPTOR"),
    ("Error'InvalidImageSize", "c'CL_INVALID_IMAGE_SIZE"),
    ("Error'InvalidSampler", "c'CL_INVALID_SAMPLER"),
    ("Error'InvalidBinary", "c'CL_INVALID_BINARY"),
    ("Error'InvalidBuildOptions", "c'CL_INVALID_BUILD_OPTIONS"),
    ("Error'InvalidProgram", "c'CL_INVALID_PROGRAM"),
    ("Error'InvalidProgramExecutable", "c'CL_INVALID_PROGRAM_EXECUTABLE"),
    ("Error'InvalidKernelName", "c'CL_INVALID_KERNEL_NAME"),
    ("Error'InvalidKernelDefinition", "c'CL_INVALID_KERNEL_DEFINITION"),
    ("Error'InvalidKernel", "c'CL_INVALID_KERNEL"),
    ("Error'InvalidArgIndex", "c'CL_INVALID_ARG_INDEX"),
    ("Error'InvalidArgValue", "c'CL_INVALID_ARG_VALUE"),
    ("Error'InvalidArgSize", "c'CL_INVALID_ARG_SIZE"),
    ("Error'InvalidKernelArgs", "c'CL_INVALID_KERNEL_ARGS"),
    ("Error'InvalidWorkDimension", "c'CL_INVALID_WORK_DIMENSION"),
    ("Error'InvalidWorkGroupSize", "c'CL_INVALID_WORK_GROUP_SIZE"),
    ("Error'InvalidWorkItemSize", "c'CL_INVALID_WORK_ITEM_SIZE"),
    ("Error'InvalidGlobalOffset", "c'CL_INVALID_GLOBAL_OFFSET"),
    ("Error'InvalidEventWaitList", "c'CL_INVALID_EVENT_WAIT_LIST"),
    ("Error'InvalidEvent", "c'CL_INVALID_EVENT"),
    ("Error'InvalidOperation", "c'CL_INVALID_OPERATION"),
    ("Error'InvalidGlObject", "c'CL_INVALID_GL_OBJECT"),
    ("Error'InvalidBufferSize", "c'CL_INVALID_BUFFER_SIZE"),
    ("Error'InvalidMipLevel", "c'CL_INVALID_MIP_LEVEL"),
    ("Error'InvalidGlobalWorkSize", "c'CL_INVALID_GLOBAL_WORK_SIZE"),
    ("Error'InvalidProperty", "c'CL_INVALID_PROPERTY"),
    ("Error'InvalidImageDescriptor", "c'CL_INVALID_IMAGE_DESCRIPTOR"),
    ("Error'InvalidCompilerOptions", "c'CL_INVALID_COMPILER_OPTIONS"),
    ("Error'InvalidLinkerOptions", "c'CL_INVALID_LINKER_OPTIONS"),
    ("Error'InvalidDevicePartitionCount", "c'CL_INVALID_DEVICE_PARTITION_COUNT"),
    ("Error'InvalidPipeSize", "c'CL_INVALID_PIPE_SIZE"),
    ("Error'InvalidDeviceQueue", "c'CL_INVALID_DEVICE_QUEUE"),
    ("Error'InvalidSpecId", "c'CL_INVALID_SPEC_ID"),
    ("Error'MaxSizeRestrictionExceeded", "c'CL_MAX_SIZE_RESTRICTION_EXCEEDED")
  ]
  ''CInt
  "Error"

convertableType
  [ ("Version'10", "c'CL_VERSION_1_0"),
    ("Version'11", "c'CL_VERSION_1_1"),
    ("Version'12", "c'CL_VERSION_1_2"),
    ("Version'20", "c'CL_VERSION_2_0"),
    ("Version'21", "c'CL_VERSION_2_1"),
    ("Version'22", "c'CL_VERSION_2_2")
  ]
  ''CUInt
  "Version"

deriveConvertable
  [ ('True, "c'CL_TRUE"),
    ('False, "c'CL_FALSE")
  ]
  ''CUInt
  ''Bool

convertableType
  [ ("BlockingMode'Blocking", "c'CL_TRUE"),
    ("BlockingMode'NonBlocking", "c'CL_FALSE")
  ]
  ''CUInt
  "BlockingMode"

convertableType
  [ ("Platform'Profile", "c'CL_PLATFORM_PROFILE"),
    ("Platform'Version", "c'CL_PLATFORM_VERSION"),
    ("Platform'Name", "c'CL_PLATFORM_NAME"),
    ("Platform'Vendor", "c'CL_PLATFORM_VENDOR"),
    ("Platform'Extensions", "c'CL_PLATFORM_EXTENSIONS"),
    ("Platform'HostTimerResolution", "c'CL_PLATFORM_HOST_TIMER_RESOLUTION")
  ]
  ''CUInt
  "PlatformInfo"

convertableTypeB
  [ ("DeviceType'Default", "c'CL_DEVICE_TYPE_DEFAULT"),
    ("DeviceType'Cpu", "c'CL_DEVICE_TYPE_CPU"),
    ("DeviceType'Gpu", "c'CL_DEVICE_TYPE_GPU"),
    ("DeviceType'Accelerator", "c'CL_DEVICE_TYPE_ACCELERATOR"),
    ("DeviceType'Custom", "c'CL_DEVICE_TYPE_CUSTOM"),
    ("DeviceType'All", "c'CL_DEVICE_TYPE_ALL")
  ]
  ''CUInt
  "DeviceType"

convertableType
  [ ("DeviceInfo'DeviceType", "c'CL_DEVICE_TYPE"),
    ("DeviceInfo'VendorId", "c'CL_DEVICE_VENDOR_ID"),
    ("DeviceInfo'MaxComputeUnits", "c'CL_DEVICE_MAX_COMPUTE_UNITS"),
    ("DeviceInfo'MaxWorkItemDimensions", "c'CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS"),
    ("DeviceInfo'MaxWorkGroupSize", "c'CL_DEVICE_MAX_WORK_GROUP_SIZE"),
    ("DeviceInfo'MaxWorkItemSizes", "c'CL_DEVICE_MAX_WORK_ITEM_SIZES"),
    ("DeviceInfo'PreferredVectorWidthChar", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR"),
    ("DeviceInfo'PreferredVectorWidthShort", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT"),
    ("DeviceInfo'PreferredVectorWidthInt", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT"),
    ("DeviceInfo'PreferredVectorWidthLong", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG"),
    ("DeviceInfo'PreferredVectorWidthFloat", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT"),
    ("DeviceInfo'PreferredVectorWidthDouble", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE"),
    ("DeviceInfo'MaxClockFrequency", "c'CL_DEVICE_MAX_CLOCK_FREQUENCY"),
    ("DeviceInfo'AddressBits", "c'CL_DEVICE_ADDRESS_BITS"),
    ("DeviceInfo'MaxReadImageArgs", "c'CL_DEVICE_MAX_READ_IMAGE_ARGS"),
    ("DeviceInfo'MaxWriteImageArgs", "c'CL_DEVICE_MAX_WRITE_IMAGE_ARGS"),
    ("DeviceInfo'MaxMemAllocSize", "c'CL_DEVICE_MAX_MEM_ALLOC_SIZE"),
    ("DeviceInfo'Image2DMaxWidth", "c'CL_DEVICE_IMAGE2D_MAX_WIDTH"),
    ("DeviceInfo'Image2DMaxHeight", "c'CL_DEVICE_IMAGE2D_MAX_HEIGHT"),
    ("DeviceInfo'Image3DMaxWidth", "c'CL_DEVICE_IMAGE3D_MAX_WIDTH"),
    ("DeviceInfo'Image3DMaxHeight", "c'CL_DEVICE_IMAGE3D_MAX_HEIGHT"),
    ("DeviceInfo'Image3DMaxDepth", "c'CL_DEVICE_IMAGE3D_MAX_DEPTH"),
    ("DeviceInfo'ImageSupport", "c'CL_DEVICE_IMAGE_SUPPORT"),
    ("DeviceInfo'MaxParameterSize", "c'CL_DEVICE_MAX_PARAMETER_SIZE"),
    ("DeviceInfo'MaxSamplers", "c'CL_DEVICE_MAX_SAMPLERS"),
    ("DeviceInfo'MemBaseAddrAlign", "c'CL_DEVICE_MEM_BASE_ADDR_ALIGN"),
    ("DeviceInfo'MinDataTypeAlignSize", "c'CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE"),
    ("DeviceInfo'SingleFpConfig", "c'CL_DEVICE_SINGLE_FP_CONFIG"),
    ("DeviceInfo'GlobalMemCacheType", "c'CL_DEVICE_GLOBAL_MEM_CACHE_TYPE"),
    ("DeviceInfo'GlobalMemCachelineSize", "c'CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE"),
    ("DeviceInfo'GlobalMemCacheSize", "c'CL_DEVICE_GLOBAL_MEM_CACHE_SIZE"),
    ("DeviceInfo'GlobalMemSize", "c'CL_DEVICE_GLOBAL_MEM_SIZE"),
    ("DeviceInfo'MaxConstantBufferSize", "c'CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE"),
    ("DeviceInfo'MaxConstantArgs", "c'CL_DEVICE_MAX_CONSTANT_ARGS"),
    ("DeviceInfo'LocalMemType", "c'CL_DEVICE_LOCAL_MEM_TYPE"),
    ("DeviceInfo'LocalMemSize", "c'CL_DEVICE_LOCAL_MEM_SIZE"),
    ("DeviceInfo'ErrorCorrectionSupport", "c'CL_DEVICE_ERROR_CORRECTION_SUPPORT"),
    ("DeviceInfo'ProfilingTimerResolution", "c'CL_DEVICE_PROFILING_TIMER_RESOLUTION"),
    ("DeviceInfo'EndianLittle", "c'CL_DEVICE_ENDIAN_LITTLE"),
    ("DeviceInfo'Available", "c'CL_DEVICE_AVAILABLE"),
    ("DeviceInfo'CompilerAvailable", "c'CL_DEVICE_COMPILER_AVAILABLE"),
    ("DeviceInfo'ExecutionCapabilities", "c'CL_DEVICE_EXECUTION_CAPABILITIES"),
    ("DeviceInfo'DeviceQueueProperties", "c'CL_DEVICE_QUEUE_PROPERTIES"),
    ("DeviceInfo'QueueOnHostProperties", "c'CL_DEVICE_QUEUE_ON_HOST_PROPERTIES"),
    ("DeviceInfo'DeviceName", "c'CL_DEVICE_NAME"),
    ("DeviceInfo'DeviceVendor", "c'CL_DEVICE_VENDOR"),
    ("DeviceInfo'DriverVersion", "c'CL_DRIVER_VERSION"),
    ("DeviceInfo'DeviceProfile", "c'CL_DEVICE_PROFILE"),
    ("DeviceInfo'DeviceVersion", "c'CL_DEVICE_VERSION"),
    ("DeviceInfo'DeviceExtensions", "c'CL_DEVICE_EXTENSIONS"),
    ("DeviceInfo'DevicePlatform", "c'CL_DEVICE_PLATFORM"),
    ("DeviceInfo'DoubleFpConfig", "c'CL_DEVICE_DOUBLE_FP_CONFIG"),
    ("DeviceInfo'HalfFpConfig", "c'CL_DEVICE_HALF_FP_CONFIG"),
    ("DeviceInfo'PreferredVectorWidthHalf", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF"),
    ("DeviceInfo'HostUnifiedMemory", "c'CL_DEVICE_HOST_UNIFIED_MEMORY"),
    ("DeviceInfo'NativeVectorWidthChar", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR"),
    ("DeviceInfo'NativeVectorWidthShort", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT"),
    ("DeviceInfo'NativeVectorWidthInt", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_INT"),
    ("DeviceInfo'NativeVectorWidthLong", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG"),
    ("DeviceInfo'NativeVectorWidthFloat", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT"),
    ("DeviceInfo'NativeVectorWidthDouble", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE"),
    ("DeviceInfo'NativeVectorWidthHalf", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF"),
    ("DeviceInfo'OpenclCVersion", "c'CL_DEVICE_OPENCL_C_VERSION"),
    ("DeviceInfo'LinkerAvailable", "c'CL_DEVICE_LINKER_AVAILABLE"),
    ("DeviceInfo'BuiltInKernels", "c'CL_DEVICE_BUILT_IN_KERNELS"),
    ("DeviceInfo'ImageMaxBufferSize", "c'CL_DEVICE_IMAGE_MAX_BUFFER_SIZE"),
    ("DeviceInfo'ImageMaxArraySize", "c'CL_DEVICE_IMAGE_MAX_ARRAY_SIZE"),
    ("DeviceInfo'DeviceParent", "c'CL_DEVICE_PARENT_DEVICE"),
    ("DeviceInfo'PartitionMaxSubs", "c'CL_DEVICE_PARTITION_MAX_SUB_DEVICES"),
    ("DeviceInfo'PartitionProperties", "c'CL_DEVICE_PARTITION_PROPERTIES"),
    ("DeviceInfo'PartitionAffinityDomain", "c'CL_DEVICE_PARTITION_AFFINITY_DOMAIN"),
    ("DeviceInfo'PartitionType", "c'CL_DEVICE_PARTITION_TYPE"),
    ("DeviceInfo'ReferenceCount", "c'CL_DEVICE_REFERENCE_COUNT"),
    ("DeviceInfo'PreferredInteropUserSync", "c'CL_DEVICE_PREFERRED_INTEROP_USER_SYNC"),
    ("DeviceInfo'PrintfBufferSize", "c'CL_DEVICE_PRINTF_BUFFER_SIZE"),
    ("DeviceInfo'ImagePitchAlignment", "c'CL_DEVICE_IMAGE_PITCH_ALIGNMENT"),
    ("DeviceInfo'ImageBaseAddressAlignment", "c'CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT"),
    ("DeviceInfo'MaxReadWriteImageArgs", "c'CL_DEVICE_MAX_READ_WRITE_IMAGE_ARGS"),
    ("DeviceInfo'MaxGlobalVariableSize", "c'CL_DEVICE_MAX_GLOBAL_VARIABLE_SIZE"),
    ("DeviceInfo'QueueOnProperties", "c'CL_DEVICE_QUEUE_ON_DEVICE_PROPERTIES"),
    ("DeviceInfo'QueueOnPreferredSize", "c'CL_DEVICE_QUEUE_ON_DEVICE_PREFERRED_SIZE"),
    ("DeviceInfo'QueueOnMaxSize", "c'CL_DEVICE_QUEUE_ON_DEVICE_MAX_SIZE"),
    ("DeviceInfo'MaxOnQueues", "c'CL_DEVICE_MAX_ON_DEVICE_QUEUES"),
    ("DeviceInfo'MaxOnEvents", "c'CL_DEVICE_MAX_ON_DEVICE_EVENTS"),
    ("DeviceInfo'SvmCapabilities", "c'CL_DEVICE_SVM_CAPABILITIES"),
    ("DeviceInfo'GlobalVariablePreferredTotalSize", "c'CL_DEVICE_GLOBAL_VARIABLE_PREFERRED_TOTAL_SIZE"),
    ("DeviceInfo'MaxPipeArgs", "c'CL_DEVICE_MAX_PIPE_ARGS"),
    ("DeviceInfo'PipeMaxActiveReservations", "c'CL_DEVICE_PIPE_MAX_ACTIVE_RESERVATIONS"),
    ("DeviceInfo'PipeMaxPacketSize", "c'CL_DEVICE_PIPE_MAX_PACKET_SIZE"),
    ("DeviceInfo'PreferredPlatformAtomicAlignment", "c'CL_DEVICE_PREFERRED_PLATFORM_ATOMIC_ALIGNMENT"),
    ("DeviceInfo'PreferredGlobalAtomicAlignment", "c'CL_DEVICE_PREFERRED_GLOBAL_ATOMIC_ALIGNMENT"),
    ("DeviceInfo'PreferredLocalAtomicAlignment", "c'CL_DEVICE_PREFERRED_LOCAL_ATOMIC_ALIGNMENT"),
    ("DeviceInfo'ILVersion", "c'CL_DEVICE_IL_VERSION"),
    ("DeviceInfo'MaxNumSubGroups", "c'CL_DEVICE_MAX_NUM_SUB_GROUPS"),
    ("DeviceInfo'SubGroupIndependentForwardProgress", "c'CL_DEVICE_SUB_GROUP_INDEPENDENT_FORWARD_PROGRESS")
  ]
  ''CUInt
  "DeviceInfo"

convertableTypeB
  [ ("FP'Denorm", "c'CL_FP_DENORM"),
    ("FP'InfNaN", "c'CL_FP_INF_NAN"),
    ("FP'RoundToNearest", "c'CL_FP_ROUND_TO_NEAREST"),
    ("FP'RoundToZero", "c'CL_FP_ROUND_TO_ZERO"),
    ("FP'RoundToInf", "c'CL_FP_ROUND_TO_INF"),
    ("FP'FMA", "c'CL_FP_FMA"),
    ("FP'SoftFloat", "c'CL_FP_SOFT_FLOAT"),
    ("FP'CorrectlyRoundedDivideSqrt", "c'CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT")
  ]
  ''CUInt
  "FPConfig"

convertableType
  [ ("MemCache'None", "c'CL_NONE"),
    ("MemCache'ReadOnly", "c'CL_READ_ONLY_CACHE"),
    ("MemCache'ReadWrite", "c'CL_READ_WRITE_CACHE")
  ]
  ''CUInt
  "MemCacheType"

convertableType
  [ ("DeviceMem'Local", "c'CL_LOCAL"),
    ("DeviceMem'Global", "c'CL_GLOBAL")
  ]
  ''CUInt
  "DeviceMemType"

convertableTypeB
  [ ("Exec'Kernel", "c'CL_EXEC_KERNEL"),
    ("Exec'NativeKernel", "c'CL_EXEC_NATIVE_KERNEL")
  ]
  ''CUInt
  "ExecCapabilities"

convertableTypeB
  [ ("QueueProp'OutOfOrderExecModeEnable", "c'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE"),
    ("QueueProp'ProfilingEnable", "c'CL_QUEUE_PROFILING_ENABLE"),
    ("QueueProp'OnDevice", "c'CL_QUEUE_ON_DEVICE"),
    ("QueueProp'OnDeviceDefault", "c'CL_QUEUE_ON_DEVICE_DEFAULT")
  ]
  ''CUInt
  "CommandQueueProperties"

convertableType
  [ ("ContextInfo'ReferenceCount", "c'CL_CONTEXT_REFERENCE_COUNT"),
    ("ContextInfo'Devices", "c'CL_CONTEXT_DEVICES"),
    ("ContextInfo'Properties", "c'CL_CONTEXT_PROPERTIES"),
    ("ContextInfo'NumDevices", "c'CL_CONTEXT_NUM_DEVICES")
  ]
  ''CUInt
  "ContextInfo"

convertableType
  [ ("ContextProp'Platform", "c'CL_CONTEXT_PLATFORM"),
    ("ContextProp'InteropUserSync", "c'CL_CONTEXT_INTEROP_USER_SYNC")
  ]
  ''CUInt
  "ContextProperties"

convertableType
  [ ("DevicePartition'Equally", "c'CL_DEVICE_PARTITION_EQUALLY"),
    ("DevicePartition'ByCounts", "c'CL_DEVICE_PARTITION_BY_COUNTS"),
    ("DevicePartition'ByCountsListEnd", "c'CL_DEVICE_PARTITION_BY_COUNTS_LIST_END"),
    ("DevicePartition'ByAffinityDomain", "c'CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN")
  ]
  ''CUInt
  "DevicePartition"

convertableType
  [ ("AffinityDomain'Numa", "c'CL_DEVICE_AFFINITY_DOMAIN_NUMA"),
    ("AffinityDomain'L4Cache", "c'CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE"),
    ("AffinityDomain'L3Cache", "c'CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE"),
    ("AffinityDomain'L2Cache", "c'CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE"),
    ("AffinityDomain'L1Cache", "c'CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE"),
    ("AffinityDomain'NextPartitionable", "c'CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE")
  ]
  ''CUInt
  "DeviceAffinityDomain"

convertableType
  [ ("SVMCap'CoarseGrainBuffer", "c'CL_DEVICE_SVM_COARSE_GRAIN_BUFFER"),
    ("SVMCap'FineGrainBuffer", "c'CL_DEVICE_SVM_FINE_GRAIN_BUFFER"),
    ("SVMCap'FineGrainSystem", "c'CL_DEVICE_SVM_FINE_GRAIN_SYSTEM"),
    ("SVMCap'Atomics", "c'CL_DEVICE_SVM_ATOMICS")
  ]
  ''CUInt
  "DeviceSVMCapabilities"

convertableType
  [ ("QueueInfo'Context", "c'CL_QUEUE_CONTEXT"),
    ("QueueInfo'Device", "c'CL_QUEUE_DEVICE"),
    ("QueueInfo'ReferenceCount", "c'CL_QUEUE_REFERENCE_COUNT"),
    ("QueueInfo'Properties", "c'CL_QUEUE_PROPERTIES"),
    ("QueueInfo'Size", "c'CL_QUEUE_SIZE"),
    ("QueueInfo'DeviceDefault", "c'CL_QUEUE_DEVICE_DEFAULT")
  ]
  ''CUInt
  "CommandQueueInfo"

convertableTypeB
  [ ("MemFlag'ReadWrite", "c'CL_MEM_READ_WRITE"),
    ("MemFlag'WriteOnly", "c'CL_MEM_WRITE_ONLY"),
    ("MemFlag'ReadOnly", "c'CL_MEM_READ_ONLY"),
    ("MemFlag'UseHostPtr", "c'CL_MEM_USE_HOST_PTR"),
    ("MemFlag'AllocHostPtr", "c'CL_MEM_ALLOC_HOST_PTR"),
    ("MemFlag'CopyHostPtr", "c'CL_MEM_COPY_HOST_PTR"),
    ("MemFlag'HostWriteOnly", "c'CL_MEM_HOST_WRITE_ONLY"),
    ("MemFlag'HostReadOnly", "c'CL_MEM_HOST_READ_ONLY"),
    ("MemFlag'HostNoAccess", "c'CL_MEM_HOST_NO_ACCESS"),
    ("MemFlag'SVMFineGrainBuffer", "c'CL_MEM_SVM_FINE_GRAIN_BUFFER"),
    ("MemFlag'SVMAtomics", "c'CL_MEM_SVM_ATOMICS"),
    ("MemFlag'KernelReadAndWrite", "c'CL_MEM_KERNEL_READ_AND_WRITE")
  ]
  ''CUInt
  "MemFlags"

convertableTypeB
  [ ("MemMigration'ObjectHost", "c'CL_MIGRATE_MEM_OBJECT_HOST"),
    ("MemMigration'ContentUndefined", "c'CL_MIGRATE_MEM_OBJECT_HOST")
  ]
  ''CUInt
  "MemMigrationFlags"

convertableType
  [ ("ChannelOrder'R", "c'CL_R"),
    ("ChannelOrder'A", "c'CL_A"),
    ("ChannelOrder'RG", "c'CL_RG"),
    ("ChannelOrder'RA", "c'CL_RA"),
    ("ChannelOrder'RGB", "c'CL_RGB"),
    ("ChannelOrder'RGBA", "c'CL_RGBA"),
    ("ChannelOrder'BGRA", "c'CL_BGRA"),
    ("ChannelOrder'ARGB", "c'CL_ARGB"),
    ("ChannelOrder'Intensity", "c'CL_INTENSITY"),
    ("ChannelOrder'Luminance", "c'CL_LUMINANCE"),
    ("ChannelOrder'Rx", "c'CL_Rx"),
    ("ChannelOrder'RGx", "c'CL_RGx"),
    ("ChannelOrder'RGBx", "c'CL_RGBx"),
    ("ChannelOrder'Depth", "c'CL_DEPTH"),
    ("ChannelOrder'DepthStencil", "c'CL_DEPTH_STENCIL"),
    ("ChannelOrder'SRGB", "c'CL_sRGB"),
    ("ChannelOrder'SRGBx", "c'CL_sRGBx"),
    ("ChannelOrder'SRGBA", "c'CL_sRGBA"),
    ("ChannelOrder'SBGRA", "c'CL_sBGRA"),
    ("ChannelOrder'ABGR", "c'CL_ABGR")
  ]
  ''CUInt
  "ChannelOrder"

convertableType
  [ ("ChannelType'SNormInt8", "c'CL_SNORM_INT8"),
    ("ChannelType'SNormInt16", "c'CL_SNORM_INT16"),
    ("ChannelType'UNormInt8", "c'CL_UNORM_INT8"),
    ("ChannelType'UNormInt16", "c'CL_UNORM_INT16"),
    ("ChannelType'UNormShort565", "c'CL_UNORM_SHORT_565"),
    ("ChannelType'UNormShort555", "c'CL_UNORM_SHORT_555"),
    ("ChannelType'UNormInt101010", "c'CL_UNORM_INT_101010"),
    ("ChannelType'SignedInt8", "c'CL_SIGNED_INT8"),
    ("ChannelType'SignedInt16", "c'CL_SIGNED_INT16"),
    ("ChannelType'SignedInt32", "c'CL_SIGNED_INT32"),
    ("ChannelType'UnsignedInt8", "c'CL_UNSIGNED_INT8"),
    ("ChannelType'UnsignedInt16", "c'CL_UNSIGNED_INT16"),
    ("ChannelType'UnsignedInt32", "c'CL_UNSIGNED_INT32"),
    ("ChannelType'HalfFloat", "c'CL_HALF_FLOAT"),
    ("ChannelType'Float", "c'CL_FLOAT"),
    ("ChannelType'UNormInt24", "c'CL_UNORM_INT24"),
    ("ChannelType'UNormInt1010102", "c'CL_UNORM_INT_101010_2")
  ]
  ''CUInt
  "ChannelType"

convertableType
  [ ("MemObject'Buffer", "c'CL_MEM_OBJECT_BUFFER"),
    ("MemObject'Image2D", "c'CL_MEM_OBJECT_IMAGE2D"),
    ("MemObject'Image3D", "c'CL_MEM_OBJECT_IMAGE3D"),
    ("MemObject'Image2DArray", "c'CL_MEM_OBJECT_IMAGE2D_ARRAY"),
    ("MemObject'Image1D", "c'CL_MEM_OBJECT_IMAGE1D"),
    ("MemObject'Image1DArray", "c'CL_MEM_OBJECT_IMAGE1D_ARRAY"),
    ("MemObject'Image1DBuffer", "c'CL_MEM_OBJECT_IMAGE1D_BUFFER"),
    ("MemObject'Pipe", "c'CL_MEM_OBJECT_PIPE")
  ]
  ''CUInt
  "MemObjectType"

convertableType
  [ ("MemInfo'Type", "c'CL_MEM_TYPE"),
    ("MemInfo'Flags", "c'CL_MEM_FLAGS"),
    ("MemInfo'Size", "c'CL_MEM_SIZE"),
    ("MemInfo'HostPtr", "c'CL_MEM_HOST_PTR"),
    ("MemInfo'MapCount", "c'CL_MEM_MAP_COUNT"),
    ("MemInfo'ReferenceCount", "c'CL_MEM_REFERENCE_COUNT"),
    ("MemInfo'Context", "c'CL_MEM_CONTEXT"),
    ("MemInfo'AssociatedMemObject", "c'CL_MEM_ASSOCIATED_MEMOBJECT"),
    ("MemInfo'Offset", "c'CL_MEM_OFFSET"),
    ("MemInfo'UsesSVMPointer", "c'CL_MEM_USES_SVM_POINTER")
  ]
  ''CUInt
  "MemInfo"

convertableType
  [ ("ImageInfo'Format", "c'CL_IMAGE_FORMAT"),
    ("ImageInfo'ElementSize", "c'CL_IMAGE_ELEMENT_SIZE"),
    ("ImageInfo'RowPitch", "c'CL_IMAGE_ROW_PITCH"),
    ("ImageInfo'SlicePitch", "c'CL_IMAGE_SLICE_PITCH"),
    ("ImageInfo'Width", "c'CL_IMAGE_WIDTH"),
    ("ImageInfo'Height", "c'CL_IMAGE_HEIGHT"),
    ("ImageInfo'Depth", "c'CL_IMAGE_DEPTH"),
    ("ImageInfo'ArraySize", "c'CL_IMAGE_ARRAY_SIZE"),
    ("ImageInfo'Buffer", "c'CL_IMAGE_BUFFER"),
    ("ImageInfo'NumMIPLevels", "c'CL_IMAGE_NUM_MIP_LEVELS"),
    ("ImageInfo'NumSamples", "c'CL_IMAGE_NUM_SAMPLES")
  ]
  ''CUInt
  "ImageInfo"

convertableType
  [ ("PipeInfo'PacketSize", "c'CL_PIPE_MAX_PACKETS"),
    ("PipeInfo'MaxPackets", "c'CL_PIPE_MAX_PACKETS")
  ]
  ''CUInt
  "PipeInfo"

convertableType
  [ ("Address'None", "c'CL_ADDRESS_NONE"),
    ("Address'ClampToEdge", "c'CL_ADDRESS_CLAMP_TO_EDGE"),
    ("Address'Clamp", "c'CL_ADDRESS_CLAMP"),
    ("Address'Repeat", "c'CL_ADDRESS_REPEAT"),
    ("Address'MirroredRepeat", "c'CL_ADDRESS_MIRRORED_REPEAT")
  ]
  ''CUInt
  "AddressingMode"

convertableType
  [ ("Filter'Nearest", "c'CL_FILTER_NEAREST"),
    ("Filter'Linear", "c'CL_FILTER_LINEAR")
  ]
  ''CUInt
  "FilterMode"

convertableType
  [ ("SamplerInfo'ReferenceCount", "c'CL_SAMPLER_REFERENCE_COUNT"),
    ("SamplerInfo'Context", "c'CL_SAMPLER_CONTEXT"),
    ("SamplerInfo'NormalizedCoords", "c'CL_SAMPLER_NORMALIZED_COORDS"),
    ("SamplerInfo'AddressingMode", "c'CL_SAMPLER_ADDRESSING_MODE"),
    ("SamplerInfo'FilterMode", "c'CL_SAMPLER_FILTER_MODE"),
    ("SamplerInfo'MIPFilterMode", "c'CL_SAMPLER_MIP_FILTER_MODE"),
    ("SamplerInfo'LODMin", "c'CL_SAMPLER_LOD_MIN"),
    ("SamplerInfo'LODMax", "c'CL_SAMPLER_LOD_MAX")
  ]
  ''CUInt
  "SamplerInfo"

convertableTypeB
  [ ("MapFlags'Read", "c'CL_MAP_READ"),
    ("MapFlags'Write", "c'CL_MAP_WRITE"),
    ("MapFlags'WriteInvalidateRegion", "c'CL_MAP_WRITE_INVALIDATE_REGION")
  ]
  ''CUInt
  "MapFlags"

convertableType
  [ ("ProgramInfo'ReferenceCount", "c'CL_PROGRAM_REFERENCE_COUNT"),
    ("ProgramInfo'Context", "c'CL_PROGRAM_CONTEXT"),
    ("ProgramInfo'NumDevices", "c'CL_PROGRAM_NUM_DEVICES"),
    ("ProgramInfo'Devices", "c'CL_PROGRAM_DEVICES"),
    ("ProgramInfo'Source", "c'CL_PROGRAM_SOURCE"),
    ("ProgramInfo'BinarySizes", "c'CL_PROGRAM_BINARY_SIZES"),
    ("ProgramInfo'Binaries", "c'CL_PROGRAM_BINARIES"),
    ("ProgramInfo'NumKernels", "c'CL_PROGRAM_NUM_KERNELS"),
    ("ProgramInfo'KernelNames", "c'CL_PROGRAM_KERNEL_NAMES"),
    ("ProgramInfo'IL", "c'CL_PROGRAM_IL"),
    ("ProgramInfo'ScopeGlobalCtorsPresent", "c'CL_PROGRAM_SCOPE_GLOBAL_CTORS_PRESENT"),
    ("ProgramInfo'ScopeGlobalDtorsPresent", "c'CL_PROGRAM_SCOPE_GLOBAL_DTORS_PRESENT")
  ]
  ''CUInt
  "ProgramInfo"

convertableType
  [ ("ProgramBuild'Status", "c'CL_PROGRAM_BUILD_STATUS"),
    ("ProgramBuild'Options", "c'CL_PROGRAM_BUILD_OPTIONS"),
    ("ProgramBuild'Log", "c'CL_PROGRAM_BUILD_LOG"),
    ("ProgramBuild'BinaryType", "c'CL_PROGRAM_BINARY_TYPE"),
    ("ProgramBuild'GlobalVariableTotalSize", "c'CL_PROGRAM_BUILD_GLOBAL_VARIABLE_TOTAL_SIZE")
  ]
  ''CUInt
  "ProgramBuildInfo"

convertableType
  [ ("Binary'None", "c'CL_PROGRAM_BINARY_TYPE_NONE"),
    ("Binary'CompiledObject", "c'CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT"),
    ("Binary'Library", "c'CL_PROGRAM_BINARY_TYPE_LIBRARY"),
    ("Binary'Executable", "c'CL_PROGRAM_BINARY_TYPE_EXECUTABLE")
  ]
  ''CUInt
  "ProgramBinaryType"

convertableType
  [ ("Build'Success", "c'CL_BUILD_SUCCESS"),
    ("Build'None", "c'CL_BUILD_NONE"),
    ("Build'Error", "c'CL_BUILD_ERROR"),
    ("Build'InProgress", "c'CL_BUILD_IN_PROGRESS")
  ]
  ''CInt
  "BuildStatus"

convertableType
  [ ("Kernel'FunctionName", "c'CL_KERNEL_FUNCTION_NAME"),
    ("Kernel'NumArgs", "c'CL_KERNEL_NUM_ARGS"),
    ("Kernel'ReferenceCount", "c'CL_KERNEL_REFERENCE_COUNT"),
    ("Kernel'Context", "c'CL_KERNEL_CONTEXT"),
    ("Kernel'Program", "c'CL_KERNEL_PROGRAM"),
    ("Kernel'Attributes", "c'CL_KERNEL_ATTRIBUTES"),
    ("Kernel'MaxNumDubGroups", "c'CL_KERNEL_MAX_NUM_SUB_GROUPS"),
    ("Kernel'CompileNumSubGroups", "c'CL_KERNEL_COMPILE_NUM_SUB_GROUPS")
  ]
  ''CUInt
  "KernelInfo"

convertableType
  [ ("KernelArg'AddressQualifier", "c'CL_KERNEL_ARG_ADDRESS_QUALIFIER"),
    ("KernelArg'AccessQualifier", "c'CL_KERNEL_ARG_ACCESS_QUALIFIER"),
    ("KernelArg'TypeName", "c'CL_KERNEL_ARG_TYPE_NAME"),
    ("KernelArg'TypeQualifier", "c'CL_KERNEL_ARG_TYPE_QUALIFIER"),
    ("KernelArg'Name", "c'CL_KERNEL_ARG_NAME")
  ]
  ''CUInt
  "KernelArgInfo"

convertableType
  [ ("ArgAddress'Global", "c'CL_KERNEL_ARG_ADDRESS_GLOBAL"),
    ("ArgAddress'Local", "c'CL_KERNEL_ARG_ADDRESS_LOCAL"),
    ("ArgAddress'Constant", "c'CL_KERNEL_ARG_ADDRESS_CONSTANT"),
    ("ArgAddress'Private", "c'CL_KERNEL_ARG_ADDRESS_PRIVATE")
  ]
  ''CUInt
  "KernelArgAddressQualifier"

convertableType
  [ ("ArgAccess'ReadOnly", "c'CL_KERNEL_ARG_ACCESS_READ_ONLY"),
    ("ArgAccess'WriteOnly", "c'CL_KERNEL_ARG_ACCESS_WRITE_ONLY"),
    ("ArgAccess'ReadWrite", "c'CL_KERNEL_ARG_ACCESS_READ_WRITE"),
    ("ArgAccess'None", "c'CL_KERNEL_ARG_ACCESS_NONE")
  ]
  ''CUInt
  "KernelArgAccessQualifier"

convertableType
  [ ("ArgType'None", "c'CL_KERNEL_ARG_TYPE_NONE"),
    ("ArgType'Const", "c'CL_KERNEL_ARG_TYPE_CONST"),
    ("ArgType'Restrict", "c'CL_KERNEL_ARG_TYPE_RESTRICT"),
    ("ArgType'Volatile", "c'CL_KERNEL_ARG_TYPE_VOLATILE"),
    ("ArgType'Pipe", "c'CL_KERNEL_ARG_TYPE_PIPE")
  ]
  ''CUInt
  "KernelArgTypeQualifier"

convertableType
  [ ("KernelWG'Size", "c'CL_KERNEL_WORK_GROUP_SIZE"),
    ("KernelWG'CompileWorkGroupSize", "c'CL_KERNEL_COMPILE_WORK_GROUP_SIZE"),
    ("KernelWG'LocalMemSize", "c'CL_KERNEL_LOCAL_MEM_SIZE"),
    ("KernelWG'PreferredWorkGroupSizeMultiple", "c'CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE"),
    ("KernelWG'PrivateMemSize", "c'CL_KERNEL_PRIVATE_MEM_SIZE"),
    ("KernelWG'GlobalWorkSize", "c'CL_KERNEL_GLOBAL_WORK_SIZE")
  ]
  ''CUInt
  "KernelWorkGroupInfo"

convertableType
  [ ("KernelSG'MaxSizeForNDRange", "c'CL_KERNEL_MAX_SUB_GROUP_SIZE_FOR_NDRANGE"),
    ("KernelSG'CountForNDRange", "c'CL_KERNEL_SUB_GROUP_COUNT_FOR_NDRANGE"),
    ("KernelSG'LocalSizeCount", "c'CL_KERNEL_LOCAL_SIZE_FOR_SUB_GROUP_COUNT")
  ]
  ''CUInt
  "KernelSubGroupInfo"

convertableType
  [ ("KernelExec'SVMPtrs", "c'CL_KERNEL_EXEC_INFO_SVM_PTRS"),
    ("KernelExec'SVMFineGrainSystem", "c'CL_KERNEL_EXEC_INFO_SVM_FINE_GRAIN_SYSTEM")
  ]
  ''CUInt
  "KernelExecInfo"

convertableType
  [ ("Event'CommandQueue", "c'CL_EVENT_COMMAND_QUEUE"),
    ("Event'CommandType", "c'CL_EVENT_COMMAND_TYPE"),
    ("Event'ReferenceCount", "c'CL_EVENT_REFERENCE_COUNT"),
    ("Event'CommandExecutionStatus", "c'CL_EVENT_COMMAND_EXECUTION_STATUS"),
    ("Event'Context", "c'CL_EVENT_CONTEXT")
  ]
  ''CUInt
  "EventInfo"

convertableType
  [ ("Command'NDRangeKernel", "c'CL_COMMAND_NDRANGE_KERNEL"),
    ("Command'Task", "c'CL_COMMAND_TASK"),
    ("Command'NativeKernel", "c'CL_COMMAND_NATIVE_KERNEL"),
    ("Command'ReadBuffer", "c'CL_COMMAND_READ_BUFFER"),
    ("Command'WriteBuffer", "c'CL_COMMAND_WRITE_BUFFER"),
    ("Command'CopyBuffer", "c'CL_COMMAND_COPY_BUFFER"),
    ("Command'ReadImage", "c'CL_COMMAND_READ_IMAGE"),
    ("Command'WriteImage", "c'CL_COMMAND_WRITE_IMAGE"),
    ("Command'CopyImage", "c'CL_COMMAND_COPY_IMAGE"),
    ("Command'CopyImageToBuffer", "c'CL_COMMAND_COPY_IMAGE_TO_BUFFER"),
    ("Command'CopyBufferToImage", "c'CL_COMMAND_COPY_BUFFER_TO_IMAGE"),
    ("Command'MapBuffer", "c'CL_COMMAND_MAP_BUFFER"),
    ("Command'MapImage", "c'CL_COMMAND_MAP_IMAGE"),
    ("Command'UnmapMemObject", "c'CL_COMMAND_UNMAP_MEM_OBJECT"),
    ("Command'Marker", "c'CL_COMMAND_MARKER"),
    ("Command'AcquireGLObjects", "c'CL_COMMAND_ACQUIRE_GL_OBJECTS"),
    ("Command'ReleaseGLObjects", "c'CL_COMMAND_RELEASE_GL_OBJECTS"),
    ("Command'ReadBufferRect", "c'CL_COMMAND_READ_BUFFER_RECT"),
    ("Command'WriteBufferRect", "c'CL_COMMAND_WRITE_BUFFER_RECT"),
    ("Command'CopyBufferRect", "c'CL_COMMAND_COPY_BUFFER_RECT"),
    ("Command'User", "c'CL_COMMAND_USER"),
    ("Command'Barrier", "c'CL_COMMAND_BARRIER"),
    ("Command'MigrateMemObjects", "c'CL_COMMAND_MIGRATE_MEM_OBJECTS"),
    ("Command'FillBuffer", "c'CL_COMMAND_FILL_BUFFER"),
    ("Command'FillImage", "c'CL_COMMAND_FILL_IMAGE"),
    ("Command'SVMFree", "c'CL_COMMAND_SVM_FREE"),
    ("Command'SVMMemcpy", "c'CL_COMMAND_SVM_MEMCPY"),
    ("Command'SVMMemfill", "c'CL_COMMAND_SVM_MEMFILL"),
    ("Command'SVMMap", "c'CL_COMMAND_SVM_MAP"),
    ("Command'SVMUnmap", "c'CL_COMMAND_SVM_UNMAP")
  ]
  ''CUInt
  "CommandType"

convertableType
  [ ("Status'Complete", "c'CL_COMPLETE"),
    ("Status'Running", "c'CL_RUNNING"),
    ("Status'Submitted", "c'CL_SUBMITTED"),
    ("Status'Queued", "c'CL_QUEUED")
  ]
  ''CUInt
  "CommandStatus"

convertableType
  [ ("BufferCreate'TypeRegion", "c'CL_BUFFER_CREATE_TYPE_REGION")
  ]
  ''CInt
  "BufferCreateType"

convertableType
  [ ("Profiling'Queued", "c'CL_PROFILING_COMMAND_QUEUED"),
    ("Profiling'Submit", "c'CL_PROFILING_COMMAND_SUBMIT"),
    ("Profiling'Start", "c'CL_PROFILING_COMMAND_START"),
    ("Profiling'End", "c'CL_PROFILING_COMMAND_END"),
    ("Profiling'Complete", "c'CL_PROFILING_COMMAND_COMPLETE")
  ]
  ''CUInt
  "ProfilingInfo"

convertableTypeO "PlatformID" "C'cl_platform_id"

convertableTypeO "DeviceID" "C'cl_device_id"

convertableTypeO "Context" "C'cl_context"

convertableTypeO "CommandQueue" "C'cl_command_queue"

convertableTypeO "Memory" "C'cl_mem"

convertableTypeO "Program" "C'cl_program"

convertableTypeO "Kernel" "C'cl_kernel"

convertableTypeO "Event" "C'cl_event"

convertableTypeO "Sampler" "C'cl_sampler"

convertableTypeO "GLSync" "C'cl_GLsync"

convertableTypeP
  [ ("channelOrder", [t|ChannelOrder|], "image_channel_order"),
    ("channelType", [t|ChannelType|], "image_channel_data_type")
  ]
  "C'cl_image_format"
  "ImageFormat"

instance Convertable CSize Word where
  fromC = fromIntegral
  toC = fromIntegral

instance Convertable CUInt Word32 where
  fromC = fromIntegral
  toC = fromIntegral

convertableTypeP
  [ ("imageType", [t|MemObjectType|], "image_type"),
    ("imageWidth", [t|Word|], "image_width"),
    ("imageHeight", [t|Word|], "image_height"),
    ("imageDepth", [t|Word|], "image_depth"),
    ("imageArraySize", [t|Word|], "image_array_size"),
    ("imageRowPitch", [t|Word|], "image_row_pitch"),
    ("imageSlicePitch", [t|Word|], "image_slice_pitch"),
    ("numMipLevels", [t|Word32|], "num_mip_levels"),
    ("numSamples", [t|Word32|], "num_samples")
    -- ("memObj", ''Memory, "")
  ]
  "C'cl_image_desc"
  "ImageDesc"

convertableTypeP
  [ ("origin", [t|Word|], "origin"),
    ("size", [t|Word|], "size")
  ]
  "C'cl_buffer_region"
  "BufferRegion"

convertableTypeP
  [ ("allocationType", [t|Word32|], "allocation_type"),
    ("hostCachePolicy", [t|Word32|], "host_cache_policy")
  ]
  "C'cl_mem_ext_host_ptr"
  "MemExtHostPtr"

instance Convertable CInt Word32 where
  fromC = fromIntegral
  toC = fromIntegral

convertableTypeP
  [ ("extHostPtr", [t|MemExtHostPtr|], "ext_host_ptr"),
    ("ionFD", [t|Word32|], "ion_filedesc"),
    ("ionHostPtr", [t|Ptr ()|], "ion_hostptr")
  ]
  "C'cl_mem_ion_host_ptr"
  "MemIonHostPtr"
{-
#callback clGetGLContextInfoKHR_fn , Ptr IntPtr -> CUInt -> CSize -> Ptr () -> Ptr CSize -> IO CInt
#callback clIcdGetPlatformIDsKHR_fn , CUInt -> Ptr <cl_platform_id> -> Ptr CUInt -> IO CInt
#callback clTerminateContextKHR_fn , Ptr <cl_context> -> IO CInt
#callback clReleaseDeviceEXT_fn , Ptr <cl_device_id> -> IO CInt
#callback clRetainDeviceEXT_fn , Ptr <cl_device_id> -> IO CInt
#callback clCreateSubDevicesEXT_fn , Ptr <cl_device_id> -> Ptr CULong -> CUInt -> Ptr <cl_device_id> -> Ptr CUInt -> IO CInt
#callback clGetKernelSubGroupInfoKHR_fn , Ptr <cl_kernel> -> Ptr <cl_device_id> -> CUInt -> CSize -> Ptr () -> CSize -> Ptr () -> Ptr CSize -> IO CInt
-}
