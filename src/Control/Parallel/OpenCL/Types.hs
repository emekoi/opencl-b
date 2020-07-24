{-# LANGUAGE TemplateHaskell #-}

module Control.Parallel.OpenCL.Types where

import Bindings.OpenCL.Constants
-- import Bindings.OpenCL.Types
import Foreign.C.Convertable
import Foreign.C.Convertable.TH

convertableType
  [ ("Success", "c'CL_SUCCESS"),
    ("DeviceNotFound", "c'CL_DEVICE_NOT_FOUND"),
    ("DeviceNotAvailable", "c'CL_DEVICE_NOT_AVAILABLE"),
    ("CompilerNotAvailable", "c'CL_COMPILER_NOT_AVAILABLE"),
    ("MemObjectAllocationFailure", "c'CL_MEM_OBJECT_ALLOCATION_FAILURE"),
    ("OutOfResources", "c'CL_OUT_OF_RESOURCES"),
    ("OutOfHostMemory", "c'CL_OUT_OF_HOST_MEMORY"),
    ("ProfilingInfoNotAvailable", "c'CL_PROFILING_INFO_NOT_AVAILABLE"),
    ("MemCopyOverlap", "c'CL_MEM_COPY_OVERLAP"),
    ("ImageFormatMismatch", "c'CL_IMAGE_FORMAT_MISMATCH"),
    ("ImageFormatNotSupported", "c'CL_IMAGE_FORMAT_NOT_SUPPORTED"),
    ("BuildProgramFailure", "c'CL_BUILD_PROGRAM_FAILURE"),
    ("MapFailure", "c'CL_MAP_FAILURE"),
    ("MisalignedSubBufferOffset", "c'CL_MISALIGNED_SUB_BUFFER_OFFSET"),
    ("ExecStatusErrorForEventsInWaitList", "c'CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST"),
    ("CompileProgramFailure", "c'CL_COMPILE_PROGRAM_FAILURE"),
    ("LinkerNotAvailable", "c'CL_LINKER_NOT_AVAILABLE"),
    ("LinkProgramFailure", "c'CL_LINK_PROGRAM_FAILURE"),
    ("DevicePartitionFailed", "c'CL_DEVICE_PARTITION_FAILED"),
    ("KernelArgInfoNotAvailable", "c'CL_KERNEL_ARG_INFO_NOT_AVAILABLE"),
    ("InvalidValue", "c'CL_INVALID_VALUE"),
    ("InvalidDeviceType", "c'CL_INVALID_DEVICE_TYPE"),
    ("InvalidPlatform", "c'CL_INVALID_PLATFORM"),
    ("InvalidDevice", "c'CL_INVALID_DEVICE"),
    ("InvalidContext", "c'CL_INVALID_CONTEXT"),
    ("InvalidQueueProperties", "c'CL_INVALID_QUEUE_PROPERTIES"),
    ("InvalidCommandQueue", "c'CL_INVALID_COMMAND_QUEUE"),
    ("InvalidHostPtr", "c'CL_INVALID_HOST_PTR"),
    ("InvalidMemObject", "c'CL_INVALID_MEM_OBJECT"),
    ("InvalidImageFormatDescriptor", "c'CL_INVALID_IMAGE_FORMAT_DESCRIPTOR"),
    ("InvalidImageSize", "c'CL_INVALID_IMAGE_SIZE"),
    ("InvalidSampler", "c'CL_INVALID_SAMPLER"),
    ("InvalidBinary", "c'CL_INVALID_BINARY"),
    ("InvalidBuildOptions", "c'CL_INVALID_BUILD_OPTIONS"),
    ("InvalidProgram", "c'CL_INVALID_PROGRAM"),
    ("InvalidProgramExecutable", "c'CL_INVALID_PROGRAM_EXECUTABLE"),
    ("InvalidKernelName", "c'CL_INVALID_KERNEL_NAME"),
    ("InvalidKernelDefinition", "c'CL_INVALID_KERNEL_DEFINITION"),
    ("InvalidKernel", "c'CL_INVALID_KERNEL"),
    ("InvalidArgIndex", "c'CL_INVALID_ARG_INDEX"),
    ("InvalidArgValue", "c'CL_INVALID_ARG_VALUE"),
    ("InvalidArgSize", "c'CL_INVALID_ARG_SIZE"),
    ("InvalidKernelArgs", "c'CL_INVALID_KERNEL_ARGS"),
    ("InvalidWorkDimension", "c'CL_INVALID_WORK_DIMENSION"),
    ("InvalidWorkGroupSize", "c'CL_INVALID_WORK_GROUP_SIZE"),
    ("InvalidWorkItemSize", "c'CL_INVALID_WORK_ITEM_SIZE"),
    ("InvalidGlobalOffset", "c'CL_INVALID_GLOBAL_OFFSET"),
    ("InvalidEventWaitList", "c'CL_INVALID_EVENT_WAIT_LIST"),
    ("InvalidEvent", "c'CL_INVALID_EVENT"),
    ("InvalidOperation", "c'CL_INVALID_OPERATION"),
    ("InvalidGlObject", "c'CL_INVALID_GL_OBJECT"),
    ("InvalidBufferSize", "c'CL_INVALID_BUFFER_SIZE"),
    ("InvalidMipLevel", "c'CL_INVALID_MIP_LEVEL"),
    ("InvalidGlobalWorkSize", "c'CL_INVALID_GLOBAL_WORK_SIZE"),
    ("InvalidProperty", "c'CL_INVALID_PROPERTY"),
    ("InvalidImageDescriptor", "c'CL_INVALID_IMAGE_DESCRIPTOR"),
    ("InvalidCompilerOptions", "c'CL_INVALID_COMPILER_OPTIONS"),
    ("InvalidLinkerOptions", "c'CL_INVALID_LINKER_OPTIONS"),
    ("InvalidDevicePartitionCount", "c'CL_INVALID_DEVICE_PARTITION_COUNT"),
    ("InvalidPipeSize", "c'CL_INVALID_PIPE_SIZE"),
    ("InvalidDeviceQueue", "c'CL_INVALID_DEVICE_QUEUE"),
    ("InvalidSpecId", "c'CL_INVALID_SPEC_ID"),
    ("MaxSizeRestrictionExceeded", "c'CL_MAX_SIZE_RESTRICTION_EXCEEDED")
  ]
  ''CInt
  "Error"

convertableType
  [ ("V10", "c'CL_VERSION_1_0"),
    ("V11", "c'CL_VERSION_1_1"),
    ("V12", "c'CL_VERSION_1_2"),
    ("V20", "c'CL_VERSION_2_0"),
    ("V21", "c'CL_VERSION_2_1"),
    ("V22", "c'CL_VERSION_2_2")
  ]
  ''CInt
  "Version"

deriveConvertable
  [ ('True, "c'CL_TRUE"),
    ('False, "c'CL_FALSE")
  ]
  ''CInt
  ''Bool

convertableType
  [ ("Blocking", "c'CL_TRUE"),
    ("NonBlocking", "c'CL_FALSE")
  ]
  ''CInt
  "BlockingMode"

convertableType
  [ ("PlatformProfile", "c'CL_PLATFORM_PROFILE"),
    ("PlatformVersion", "c'CL_PLATFORM_VERSION"),
    ("PlatformName", "c'CL_PLATFORM_NAME"),
    ("PlatformVendor", "c'CL_PLATFORM_VENDOR"),
    ("PlatformExtensions", "c'CL_PLATFORM_EXTENSIONS"),
    ("HostTimerResolution", "c'CL_PLATFORM_HOST_TIMER_RESOLUTION")
  ]
  ''CInt
  "PlatformInfo"

convertableTypeB
  [ ("DeviceTypeDefault", "c'CL_DEVICE_TYPE_DEFAULT"),
    ("DeviceTypeCpu", "c'CL_DEVICE_TYPE_CPU"),
    ("DeviceTypeGpu", "c'CL_DEVICE_TYPE_GPU"),
    ("DeviceTypeAccelerator", "c'CL_DEVICE_TYPE_ACCELERATOR"),
    ("DeviceTypeCustom", "c'CL_DEVICE_TYPE_CUSTOM"),
    ("DeviceTypeAll", "c'CL_DEVICE_TYPE_ALL")
  ]
  ''CInt
  "DeviceType"

convertableType
  [ ("DeviceType", "c'CL_DEVICE_TYPE"),
    ("VendorId", "c'CL_DEVICE_VENDOR_ID"),
    ("MaxComputeUnits", "c'CL_DEVICE_MAX_COMPUTE_UNITS"),
    ("MaxWorkItemDimensions", "c'CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS"),
    ("MaxWorkGroupSize", "c'CL_DEVICE_MAX_WORK_GROUP_SIZE"),
    ("MaxWorkItemSizes", "c'CL_DEVICE_MAX_WORK_ITEM_SIZES"),
    ("PreferredVectorWidthChar", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR"),
    ("PreferredVectorWidthShort", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT"),
    ("PreferredVectorWidthInt", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT"),
    ("PreferredVectorWidthLong", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG"),
    ("PreferredVectorWidthFloat", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT"),
    ("PreferredVectorWidthDouble", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE"),
    ("MaxClockFrequency", "c'CL_DEVICE_MAX_CLOCK_FREQUENCY"),
    ("AddressBits", "c'CL_DEVICE_ADDRESS_BITS"),
    ("MaxReadImageArgs", "c'CL_DEVICE_MAX_READ_IMAGE_ARGS"),
    ("MaxWriteImageArgs", "c'CL_DEVICE_MAX_WRITE_IMAGE_ARGS"),
    ("MaxMemAllocSize", "c'CL_DEVICE_MAX_MEM_ALLOC_SIZE"),
    ("Image2DMaxWidth", "c'CL_DEVICE_IMAGE2D_MAX_WIDTH"),
    ("Image2DMaxHeight", "c'CL_DEVICE_IMAGE2D_MAX_HEIGHT"),
    ("Image3DMaxWidth", "c'CL_DEVICE_IMAGE3D_MAX_WIDTH"),
    ("Image3DMaxHeight", "c'CL_DEVICE_IMAGE3D_MAX_HEIGHT"),
    ("Image3DMaxDepth", "c'CL_DEVICE_IMAGE3D_MAX_DEPTH"),
    ("ImageSupport", "c'CL_DEVICE_IMAGE_SUPPORT"),
    ("MaxParameterSize", "c'CL_DEVICE_MAX_PARAMETER_SIZE"),
    ("MaxSamplers", "c'CL_DEVICE_MAX_SAMPLERS"),
    ("MemBaseAddrAlign", "c'CL_DEVICE_MEM_BASE_ADDR_ALIGN"),
    ("MinDataTypeAlignSize", "c'CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE"),
    ("SingleFpConfig", "c'CL_DEVICE_SINGLE_FP_CONFIG"),
    ("GlobalMemCacheType", "c'CL_DEVICE_GLOBAL_MEM_CACHE_TYPE"),
    ("GlobalMemCachelineSize", "c'CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE"),
    ("GlobalMemCacheSize", "c'CL_DEVICE_GLOBAL_MEM_CACHE_SIZE"),
    ("GlobalMemSize", "c'CL_DEVICE_GLOBAL_MEM_SIZE"),
    ("MaxConstantBufferSize", "c'CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE"),
    ("MaxConstantArgs", "c'CL_DEVICE_MAX_CONSTANT_ARGS"),
    ("LocalMemType", "c'CL_DEVICE_LOCAL_MEM_TYPE"),
    ("LocalMemSize", "c'CL_DEVICE_LOCAL_MEM_SIZE"),
    ("ErrorCorrectionSupport", "c'CL_DEVICE_ERROR_CORRECTION_SUPPORT"),
    ("ProfilingTimerResolution", "c'CL_DEVICE_PROFILING_TIMER_RESOLUTION"),
    ("EndianLittle", "c'CL_DEVICE_ENDIAN_LITTLE"),
    ("Available", "c'CL_DEVICE_AVAILABLE"),
    ("CompilerAvailable", "c'CL_DEVICE_COMPILER_AVAILABLE"),
    ("ExecutionCapabilities", "c'CL_DEVICE_EXECUTION_CAPABILITIES"),
    ("DeviceQueueProperties", "c'CL_DEVICE_QUEUE_PROPERTIES"),
    ("QueueOnHostProperties", "c'CL_DEVICE_QUEUE_ON_HOST_PROPERTIES"),
    ("DeviceName", "c'CL_DEVICE_NAME"),
    ("DeviceVendor", "c'CL_DEVICE_VENDOR"),
    ("DriverVersion", "c'CL_DRIVER_VERSION"),
    ("DeviceProfile", "c'CL_DEVICE_PROFILE"),
    ("DeviceVersion", "c'CL_DEVICE_VERSION"),
    ("DeviceExtensions", "c'CL_DEVICE_EXTENSIONS"),
    ("DevicePlatform", "c'CL_DEVICE_PLATFORM"),
    ("DoubleFpConfig", "c'CL_DEVICE_DOUBLE_FP_CONFIG"),
    ("HalfFpConfig", "c'CL_DEVICE_HALF_FP_CONFIG"),
    ("PreferredVectorWidthHalf", "c'CL_DEVICE_PREFERRED_VECTOR_WIDTH_HALF"),
    ("HostUnifiedMemory", "c'CL_DEVICE_HOST_UNIFIED_MEMORY"),
    ("NativeVectorWidthChar", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_CHAR"),
    ("NativeVectorWidthShort", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_SHORT"),
    ("NativeVectorWidthInt", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_INT"),
    ("NativeVectorWidthLong", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_LONG"),
    ("NativeVectorWidthFloat", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_FLOAT"),
    ("NativeVectorWidthDouble", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_DOUBLE"),
    ("NativeVectorWidthHalf", "c'CL_DEVICE_NATIVE_VECTOR_WIDTH_HALF"),
    ("OpenclCVersion", "c'CL_DEVICE_OPENCL_C_VERSION"),
    ("LinkerAvailable", "c'CL_DEVICE_LINKER_AVAILABLE"),
    ("BuiltInKernels", "c'CL_DEVICE_BUILT_IN_KERNELS"),
    ("ImageMaxBufferSize", "c'CL_DEVICE_IMAGE_MAX_BUFFER_SIZE"),
    ("ImageMaxArraySize", "c'CL_DEVICE_IMAGE_MAX_ARRAY_SIZE"),
    ("DeviceParent", "c'CL_DEVICE_PARENT_DEVICE"),
    ("PartitionMaxSubs", "c'CL_DEVICE_PARTITION_MAX_SUB_DEVICES"),
    ("PartitionProperties", "c'CL_DEVICE_PARTITION_PROPERTIES"),
    ("PartitionAffinityDomain", "c'CL_DEVICE_PARTITION_AFFINITY_DOMAIN"),
    ("PartitionType", "c'CL_DEVICE_PARTITION_TYPE"),
    ("ReferenceCount", "c'CL_DEVICE_REFERENCE_COUNT"),
    ("PreferredInteropUserSync", "c'CL_DEVICE_PREFERRED_INTEROP_USER_SYNC"),
    ("PrintfBufferSize", "c'CL_DEVICE_PRINTF_BUFFER_SIZE"),
    ("ImagePitchAlignment", "c'CL_DEVICE_IMAGE_PITCH_ALIGNMENT"),
    ("ImageBaseAddressAlignment", "c'CL_DEVICE_IMAGE_BASE_ADDRESS_ALIGNMENT"),
    ("MaxReadWriteImageArgs", "c'CL_DEVICE_MAX_READ_WRITE_IMAGE_ARGS"),
    ("MaxGlobalVariableSize", "c'CL_DEVICE_MAX_GLOBAL_VARIABLE_SIZE"),
    ("QueueOnProperties", "c'CL_DEVICE_QUEUE_ON_DEVICE_PROPERTIES"),
    ("QueueOnPreferredSize", "c'CL_DEVICE_QUEUE_ON_DEVICE_PREFERRED_SIZE"),
    ("QueueOnMaxSize", "c'CL_DEVICE_QUEUE_ON_DEVICE_MAX_SIZE"),
    ("MaxOnQueues", "c'CL_DEVICE_MAX_ON_DEVICE_QUEUES"),
    ("MaxOnEvents", "c'CL_DEVICE_MAX_ON_DEVICE_EVENTS"),
    ("SvmCapabilities", "c'CL_DEVICE_SVM_CAPABILITIES"),
    ("GlobalVariablePreferredTotalSize", "c'CL_DEVICE_GLOBAL_VARIABLE_PREFERRED_TOTAL_SIZE"),
    ("MaxPipeArgs", "c'CL_DEVICE_MAX_PIPE_ARGS"),
    ("PipeMaxActiveReservations", "c'CL_DEVICE_PIPE_MAX_ACTIVE_RESERVATIONS"),
    ("PipeMaxPacketSize", "c'CL_DEVICE_PIPE_MAX_PACKET_SIZE"),
    ("PreferredPlatformAtomicAlignment", "c'CL_DEVICE_PREFERRED_PLATFORM_ATOMIC_ALIGNMENT"),
    ("PreferredGlobalAtomicAlignment", "c'CL_DEVICE_PREFERRED_GLOBAL_ATOMIC_ALIGNMENT"),
    ("PreferredLocalAtomicAlignment", "c'CL_DEVICE_PREFERRED_LOCAL_ATOMIC_ALIGNMENT"),
    ("ILVersion", "c'CL_DEVICE_IL_VERSION"),
    ("MaxNumSubGroups", "c'CL_DEVICE_MAX_NUM_SUB_GROUPS"),
    ("SubGroupIndependentForwardProgress", "c'CL_DEVICE_SUB_GROUP_INDEPENDENT_FORWARD_PROGRESS")
  ]
  ''CInt
  "DeviceInfo"

convertableTypeB
  [ ("Denorm", "c'CL_FP_DENORM"),
    ("InfNaN", "c'CL_FP_INF_NAN"),
    ("RoundToNearest", "c'CL_FP_ROUND_TO_NEAREST"),
    ("RoundToZero", "c'CL_FP_ROUND_TO_ZERO"),
    ("RoundToInf", "c'CL_FP_ROUND_TO_INF"),
    ("FMA", "c'CL_FP_FMA"),
    ("SoftFloat", "c'CL_FP_SOFT_FLOAT"),
    ("CorrectlyRoundedDivideSqrt", "c'CL_FP_CORRECTLY_ROUNDED_DIVIDE_SQRT")
  ]
  ''CInt
  "FPConfig"

convertableType
  [ ("None", "c'CL_NONE"),
    ("ReadOnly", "c'CL_READ_ONLY_CACHE"),
    ("ReadWrite", "c'CL_READ_WRITE_CACHE")
  ]
  ''CInt
  "MemCacheType"

convertableType
  [ ("Local", "c'CL_LOCAL"),
    ("Global", "c'CL_GLOBAL")
  ]
  ''CInt
  "DeviceMemType"

convertableTypeB
  [ ("ExecKernel", "c'CL_EXEC_KERNEL"),
    ("ExecNativeKernel", "c'CL_EXEC_NATIVE_KERNEL")
  ]
  ''CInt
  "ExecCapabilities"

convertableTypeB
  [ ("QueueOutOfOrderExecModeEnable", "c'CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE"),
    ("QueueProfilingEnable", "c'CL_QUEUE_PROFILING_ENABLE"),
    ("QueueOnDevice", "c'CL_QUEUE_ON_DEVICE"),
    ("QueueOnDeviceDefault", "c'CL_QUEUE_ON_DEVICE_DEFAULT")
  ]
  ''CInt
  "CommandQueueProperties"

convertableType
  [ ("ContextReferenceCount", "c'CL_CONTEXT_REFERENCE_COUNT"),
    ("ContextDevices", "c'CL_CONTEXT_DEVICES"),
    ("ContextProperties", "c'CL_CONTEXT_PROPERTIES"),
    ("ContextNumDevices", "c'CL_CONTEXT_NUM_DEVICES")
  ]
  ''CInt
  "ContextInfo"

convertableType
  [ ("ContextPlatform", "c'CL_CONTEXT_PLATFORM"),
    ("ContextInteropUserSync", "c'CL_CONTEXT_INTEROP_USER_SYNC")
  ]
  ''CInt
  "ContextProperties"

convertableType
  [ ("Equally", "c'CL_DEVICE_PARTITION_EQUALLY"),
    ("ByCounts", "c'CL_DEVICE_PARTITION_BY_COUNTS"),
    ("ByCountsListEnd", "c'CL_DEVICE_PARTITION_BY_COUNTS_LIST_END"),
    ("ByAffinityDomain", "c'CL_DEVICE_PARTITION_BY_AFFINITY_DOMAIN")
  ]
  ''CInt
  "DevicePartitionProperty"

convertableType
  [ ("Numa", "c'CL_DEVICE_AFFINITY_DOMAIN_NUMA"),
    ("L4Cache", "c'CL_DEVICE_AFFINITY_DOMAIN_L4_CACHE"),
    ("L3Cache", "c'CL_DEVICE_AFFINITY_DOMAIN_L3_CACHE"),
    ("L2Cache", "c'CL_DEVICE_AFFINITY_DOMAIN_L2_CACHE"),
    ("L1Cache", "c'CL_DEVICE_AFFINITY_DOMAIN_L1_CACHE"),
    ("NextPartitionable", "c'CL_DEVICE_AFFINITY_DOMAIN_NEXT_PARTITIONABLE")
  ]
  ''CInt
  "DeviceAffinityDomain"

convertableType
  [ ("CoarseGrainBuffer", "c'CL_DEVICE_SVM_COARSE_GRAIN_BUFFER"),
    ("FineGrainBuffer", "c'CL_DEVICE_SVM_FINE_GRAIN_BUFFER"),
    ("FineGrainSystem", "c'CL_DEVICE_SVM_FINE_GRAIN_SYSTEM"),
    ("Atomics", "c'CL_DEVICE_SVM_ATOMICS")
  ]
  ''CInt
  "DeviceSVMCapabilities"

convertableType
  [ ("QueueContext", "c'CL_QUEUE_CONTEXT"),
    ("QueueDevice", "c'CL_QUEUE_DEVICE"),
    ("QueueReferenceCount", "c'CL_QUEUE_REFERENCE_COUNT"),
    ("QueueProperties", "c'CL_QUEUE_PROPERTIES"),
    ("QueueSize", "c'CL_QUEUE_SIZE"),
    ("QueueDeviceDefault", "c'CL_QUEUE_DEVICE_DEFAULT")
  ]
  ''CInt
  "CommandQueueInfo"

convertableTypeB
  [ ("MemReadWrite", "c'CL_MEM_READ_WRITE"),
    ("MemWriteOnly", "c'CL_MEM_WRITE_ONLY"),
    ("MemReadOnly", "c'CL_MEM_READ_ONLY"),
    ("MemUseHostPtr", "c'CL_MEM_USE_HOST_PTR"),
    ("MemAllocHostPtr", "c'CL_MEM_ALLOC_HOST_PTR"),
    ("MemCopyHostPtr", "c'CL_MEM_COPY_HOST_PTR"),
    ("MemHostWriteOnly", "c'CL_MEM_HOST_WRITE_ONLY"),
    ("MemHostReadOnly", "c'CL_MEM_HOST_READ_ONLY"),
    ("MemHostNoAccess", "c'CL_MEM_HOST_NO_ACCESS"),
    ("MemSVMFineGrainBuffer", "c'CL_MEM_SVM_FINE_GRAIN_BUFFER"),
    ("MemSVMAtomics", "c'CL_MEM_SVM_ATOMICS"),
    ("MemKernelReadAndWrite", "c'CL_MEM_KERNEL_READ_AND_WRITE")
  ]
  ''CInt
  "MemFlags"

convertableTypeB
  [ ("ObjectHost", "c'CL_MIGRATE_MEM_OBJECT_HOST"),
    ("ContentUndefined", "c'CL_MIGRATE_MEM_OBJECT_HOST")
  ]
  ''CInt
  "MemMigrationFlags"

convertableType
  [ ("R", "c'CL_R"),
    ("A", "c'CL_A"),
    ("RG", "c'CL_RG"),
    ("RA", "c'CL_RA"),
    ("RGB", "c'CL_RGB"),
    ("RGBA", "c'CL_RGBA"),
    ("BGRA", "c'CL_BGRA"),
    ("ARGB", "c'CL_ARGB"),
    ("Intensity", "c'CL_INTENSITY"),
    ("Luminance", "c'CL_LUMINANCE"),
    ("Rx", "c'CL_Rx"),
    ("RGx", "c'CL_RGx"),
    ("RGBx", "c'CL_RGBx"),
    ("Depth", "c'CL_DEPTH"),
    ("DepthStencil", "c'CL_DEPTH_STENCIL"),
    ("SRGB", "c'CL_sRGB"),
    ("SRGBx", "c'CL_sRGBx"),
    ("SRGBA", "c'CL_sRGBA"),
    ("SBGRA", "c'CL_sBGRA"),
    ("ABGR", "c'CL_ABGR")
  ]
  ''CInt
  "ChannelOrder"

convertableType
  [ ("SNormInt8", "c'CL_SNORM_INT8"),
    ("SNormInt16", "c'CL_SNORM_INT16"),
    ("UNormInt8", "c'CL_UNORM_INT8"),
    ("UNormInt16", "c'CL_UNORM_INT16"),
    ("UNormShort565", "c'CL_UNORM_SHORT_565"),
    ("UNormShort555", "c'CL_UNORM_SHORT_555"),
    ("UNormInt101010", "c'CL_UNORM_INT_101010"),
    ("SignedInt8", "c'CL_SIGNED_INT8"),
    ("SignedInt16", "c'CL_SIGNED_INT16"),
    ("SignedInt32", "c'CL_SIGNED_INT32"),
    ("UnsignedInt8", "c'CL_UNSIGNED_INT8"),
    ("UnsignedInt16", "c'CL_UNSIGNED_INT16"),
    ("UnsignedInt32", "c'CL_UNSIGNED_INT32"),
    ("HalfFloat", "c'CL_HALF_FLOAT"),
    ("Float", "c'CL_FLOAT"),
    ("UNormInt24", "c'CL_UNORM_INT24"),
    ("UNormInt1010102", "c'CL_UNORM_INT_101010_2")
  ]
  ''CInt
  "ChannelType"

convertableType
  [ ("Buffer", "c'CL_MEM_OBJECT_BUFFER"),
    ("Image2D", "c'CL_MEM_OBJECT_IMAGE2D"),
    ("Image3D", "c'CL_MEM_OBJECT_IMAGE3D"),
    ("Image2DArray", "c'CL_MEM_OBJECT_IMAGE2D_ARRAY"),
    ("Image1D", "c'CL_MEM_OBJECT_IMAGE1D"),
    ("Image1DArray", "c'CL_MEM_OBJECT_IMAGE1D_ARRAY"),
    ("Image1DBuffer", "c'CL_MEM_OBJECT_IMAGE1D_BUFFER"),
    ("Pipe", "c'CL_MEM_OBJECT_PIPE")
  ]
  ''CInt
  "MemObjectType"

convertableType
  [ ("MemType", "c'CL_MEM_TYPE"),
    ("MemFlags", "c'CL_MEM_FLAGS"),
    ("MemSize", "c'CL_MEM_SIZE"),
    ("MemHostPtr", "c'CL_MEM_HOST_PTR"),
    ("MemMapCount", "c'CL_MEM_MAP_COUNT"),
    ("MemReferenceCount", "c'CL_MEM_REFERENCE_COUNT"),
    ("MemContext", "c'CL_MEM_CONTEXT"),
    ("MemAssociatedMemObject", "c'CL_MEM_ASSOCIATED_MEMOBJECT"),
    ("MemOffset", "c'CL_MEM_OFFSET"),
    ("MemUsesSVMPointer", "c'CL_MEM_USES_SVM_POINTER")
  ]
  ''CInt
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
  ''CInt
  "ImageInfo"

convertableType
  [ ("PipePacketSize", "c'CL_PIPE_MAX_PACKETS"),
    ("PipeMaxPackets", "c'CL_PIPE_MAX_PACKETS")
  ]
  ''CInt
  "PipeInfo"

convertableType
  [ ("AddressNone", "c'CL_ADDRESS_NONE"),
    ("AddressClampToEdge", "c'CL_ADDRESS_CLAMP_TO_EDGE"),
    ("AddressClamp", "c'CL_ADDRESS_CLAMP"),
    ("AddressRepeat", "c'CL_ADDRESS_REPEAT"),
    ("AddressMirroredRepeat", "c'CL_ADDRESS_MIRRORED_REPEAT")
  ]
  ''CInt
  "AddressingMode"

convertableType
  [ ("FilterNearest", "c'CL_FILTER_NEAREST"),
    ("FilterLinear", "c'CL_FILTER_LINEAR")
  ]
  ''CInt
  "FilterMode"

convertableType
  [ ("SamplerReferenceCount", "c'CL_SAMPLER_REFERENCE_COUNT"),
    ("SamplerContext", "c'CL_SAMPLER_CONTEXT"),
    ("SamplerNormalizedCoords", "c'CL_SAMPLER_NORMALIZED_COORDS"),
    ("SamplerAddressingMode", "c'CL_SAMPLER_ADDRESSING_MODE"),
    ("SamplerFilterMode", "c'CL_SAMPLER_FILTER_MODE"),
    ("SamplerMIPFilterMode", "c'CL_SAMPLER_MIP_FILTER_MODE"),
    ("SamplerLODMin", "c'CL_SAMPLER_LOD_MIN"),
    ("SamplerLODMax", "c'CL_SAMPLER_LOD_MAX")
  ]
  ''CInt
  "SamplerInfo"

convertableTypeB
  [ ("MapRead", "c'CL_MAP_READ"),
    ("MapWrite", "c'CL_MAP_WRITE"),
    ("MapWriteInvalidateRegion", "c'CL_MAP_WRITE_INVALIDATE_REGION")
  ]
  ''CInt
  "MapFlags"

convertableType
  [ ("ProgramReferenceCount", "c'CL_PROGRAM_REFERENCE_COUNT"),
    ("ProgramContext", "c'CL_PROGRAM_CONTEXT"),
    ("ProgramNumDevices", "c'CL_PROGRAM_NUM_DEVICES"),
    ("ProgramDevices", "c'CL_PROGRAM_DEVICES"),
    ("ProgramSource", "c'CL_PROGRAM_SOURCE"),
    ("ProgramBinarySizes", "c'CL_PROGRAM_BINARY_SIZES"),
    ("ProgramBinaries", "c'CL_PROGRAM_BINARIES"),
    ("ProgramNumKernels", "c'CL_PROGRAM_NUM_KERNELS"),
    ("ProgramKernelNames", "c'CL_PROGRAM_KERNEL_NAMES"),
    ("ProgramIL", "c'CL_PROGRAM_IL"),
    ("ProgramScopeGlobalCtorsPresent", "c'CL_PROGRAM_SCOPE_GLOBAL_CTORS_PRESENT"),
    ("ProgramScopeGlobalDtorsPresent", "c'CL_PROGRAM_SCOPE_GLOBAL_DTORS_PRESENT")
  ]
  ''CInt
  "ProgramInfo"

convertableType
  [ ("BuildStatus", "c'CL_PROGRAM_BUILD_STATUS"),
    ("BuildOptions", "c'CL_PROGRAM_BUILD_OPTIONS"),
    ("BuildLog", "c'CL_PROGRAM_BUILD_LOG"),
    ("BinaryType", "c'CL_PROGRAM_BINARY_TYPE"),
    ("BuildGlobalVariableTotalSize", "c'CL_PROGRAM_BUILD_GLOBAL_VARIABLE_TOTAL_SIZE")
  ]
  ''CInt
  "ProgramBuildInfo"

convertableType
  [ ("BinaryTypeNone", "c'CL_PROGRAM_BINARY_TYPE_NONE"),
    ("BinaryTypeCompiledObject", "c'CL_PROGRAM_BINARY_TYPE_COMPILED_OBJECT"),
    ("BinaryTypeLibrary", "c'CL_PROGRAM_BINARY_TYPE_LIBRARY"),
    ("BinaryTypeExecutable", "c'CL_PROGRAM_BINARY_TYPE_EXECUTABLE")
  ]
  ''CInt
  "ProgramBinaryType"

convertableType
  [ ("BuildSuccess", "c'CL_BUILD_SUCCESS"),
    ("BuildNone", "c'CL_BUILD_NONE"),
    ("BuildError", "c'CL_BUILD_ERROR"),
    ("BuildInProgress", "c'CL_BUILD_IN_PROGRESS")
  ]
  ''CInt
  "BuildStatus"

convertableType
  [ ("KernelFunctionName", "c'CL_KERNEL_FUNCTION_NAME"),
    ("KernelNumArgs", "c'CL_KERNEL_NUM_ARGS"),
    ("KernelReferenceCount", "c'CL_KERNEL_REFERENCE_COUNT"),
    ("KernelContext", "c'CL_KERNEL_CONTEXT"),
    ("KernelProgram", "c'CL_KERNEL_PROGRAM"),
    ("KernelAttributes", "c'CL_KERNEL_ATTRIBUTES"),
    ("KernelMaxNumDubGroups", "c'CL_KERNEL_MAX_NUM_SUB_GROUPS"),
    ("KernelCompileNumSubGroups", "c'CL_KERNEL_COMPILE_NUM_SUB_GROUPS")
  ]
  ''CInt
  "KernelInfo"

convertableType
  [ ("KernelArgAddressQualifier", "c'CL_KERNEL_ARG_ADDRESS_QUALIFIER"),
    ("KernelArgAccessQualifier", "c'CL_KERNEL_ARG_ACCESS_QUALIFIER"),
    ("KernelArgTypeName", "c'CL_KERNEL_ARG_TYPE_NAME"),
    ("KernelArgTypeQualifier", "c'CL_KERNEL_ARG_TYPE_QUALIFIER"),
    ("KernelArgName", "c'CL_KERNEL_ARG_NAME")
  ]
  ''CInt
  "KernelArgInfo"

convertableType
  [ ("AddressGlobal", "c'CL_KERNEL_ARG_ADDRESS_GLOBAL"),
    ("AddressLocal", "c'CL_KERNEL_ARG_ADDRESS_LOCAL"),
    ("AddressConstant", "c'CL_KERNEL_ARG_ADDRESS_CONSTANT"),
    ("AddressPrivate", "c'CL_KERNEL_ARG_ADDRESS_PRIVATE")
  ]
  ''CInt
  "KernelArgAddressQualifier"

convertableType
  [ ("AccessReadOnly", "c'CL_KERNEL_ARG_ACCESS_READ_ONLY"),
    ("AccessWriteOnly", "c'CL_KERNEL_ARG_ACCESS_WRITE_ONLY"),
    ("AccessReadWrite", "c'CL_KERNEL_ARG_ACCESS_READ_WRITE"),
    ("AccessNone", "c'CL_KERNEL_ARG_ACCESS_NONE")
  ]
  ''CInt
  "KernelArgAccessQualifier"

convertableType
  [ ("TypeNone", "c'CL_KERNEL_ARG_TYPE_NONE"),
    ("TypeConst", "c'CL_KERNEL_ARG_TYPE_CONST"),
    ("TypeRestrict", "c'CL_KERNEL_ARG_TYPE_RESTRICT"),
    ("TypeVolatile", "c'CL_KERNEL_ARG_TYPE_VOLATILE"),
    ("TypePipe", "c'CL_KERNEL_ARG_TYPE_PIPE")
  ]
  ''CInt
  "KernelArgTypeQualifier"

convertableType
  [ ("KernelWorkGroupSize", "c'CL_KERNEL_WORK_GROUP_SIZE"),
    ("KernelCompileWorkGroupSize", "c'CL_KERNEL_COMPILE_WORK_GROUP_SIZE"),
    ("KernelLocalMemSize", "c'CL_KERNEL_LOCAL_MEM_SIZE"),
    ("KernelPreferredWorkGroupSizeMultiple", "c'CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE"),
    ("KernelPrivateMemSize", "c'CL_KERNEL_PRIVATE_MEM_SIZE"),
    ("KernelGlobalWorkSize", "c'CL_KERNEL_GLOBAL_WORK_SIZE")
  ]
  ''CInt
  "KernelWorkGroupInfo"

convertableType
  [ ("KernelMaxSubGroupSizeForNDRange", "c'CL_KERNEL_MAX_SUB_GROUP_SIZE_FOR_NDRANGE"),
    ("KernelSubGroupCountForNDRange", "c'CL_KERNEL_SUB_GROUP_COUNT_FOR_NDRANGE"),
    ("KernelLocalSizeForSubGroupCount", "c'CL_KERNEL_LOCAL_SIZE_FOR_SUB_GROUP_COUNT")
  ]
  ''CInt
  "KernelSubGroupInfo"

convertableType
  [ ("KernelExecInfoSVMPtrs", "c'CL_KERNEL_EXEC_INFO_SVM_PTRS"),
    ("KernelExecInfoSVMFineGrainSystem", "c'CL_KERNEL_EXEC_INFO_SVM_FINE_GRAIN_SYSTEM")
  ]
  ''CInt
  "KernelExecInfo"

convertableType
  [ ("EventCommandQueue", "c'CL_EVENT_COMMAND_QUEUE"),
    ("EventCommandType", "c'CL_EVENT_COMMAND_TYPE"),
    ("EventReferenceCount", "c'CL_EVENT_REFERENCE_COUNT"),
    ("EventCommandExecutionStatus", "c'CL_EVENT_COMMAND_EXECUTION_STATUS"),
    ("EventContext", "c'CL_EVENT_CONTEXT")
  ]
  ''CInt
  "EventInfo"

convertableType
  [ ("CommandNDRangeKernel", "c'CL_COMMAND_NDRANGE_KERNEL"),
    ("CommandTask", "c'CL_COMMAND_TASK"),
    ("CommandNativeKernel", "c'CL_COMMAND_NATIVE_KERNEL"),
    ("CommandReadBuffer", "c'CL_COMMAND_READ_BUFFER"),
    ("CommandWriteBuffer", "c'CL_COMMAND_WRITE_BUFFER"),
    ("CommandCopyBuffer", "c'CL_COMMAND_COPY_BUFFER"),
    ("CommandReadImage", "c'CL_COMMAND_READ_IMAGE"),
    ("CommandWriteImage", "c'CL_COMMAND_WRITE_IMAGE"),
    ("CommandCopyImage", "c'CL_COMMAND_COPY_IMAGE"),
    ("CommandCopyImageToBuffer", "c'CL_COMMAND_COPY_IMAGE_TO_BUFFER"),
    ("CommandCopyBufferToImage", "c'CL_COMMAND_COPY_BUFFER_TO_IMAGE"),
    ("CommandMapBuffer", "c'CL_COMMAND_MAP_BUFFER"),
    ("CommandMapImage", "c'CL_COMMAND_MAP_IMAGE"),
    ("CommandUnmapMemObject", "c'CL_COMMAND_UNMAP_MEM_OBJECT"),
    ("CommandMarker", "c'CL_COMMAND_MARKER"),
    ("CommandAcquireGLObjects", "c'CL_COMMAND_ACQUIRE_GL_OBJECTS"),
    ("CommandReleaseGLObjects", "c'CL_COMMAND_RELEASE_GL_OBJECTS"),
    ("CommandReadBufferRect", "c'CL_COMMAND_READ_BUFFER_RECT"),
    ("CommandWriteBufferRect", "c'CL_COMMAND_WRITE_BUFFER_RECT"),
    ("CommandCopyBufferRect", "c'CL_COMMAND_COPY_BUFFER_RECT"),
    ("CommandUser", "c'CL_COMMAND_USER"),
    ("CommandBarrier", "c'CL_COMMAND_BARRIER"),
    ("CommandMigrateMemObjects", "c'CL_COMMAND_MIGRATE_MEM_OBJECTS"),
    ("CommandFillBuffer", "c'CL_COMMAND_FILL_BUFFER"),
    ("CommandFillImage", "c'CL_COMMAND_FILL_IMAGE"),
    ("CommandSVMFree", "c'CL_COMMAND_SVM_FREE"),
    ("CommandSVMMemcpy", "c'CL_COMMAND_SVM_MEMCPY"),
    ("CommandSVMMemfill", "c'CL_COMMAND_SVM_MEMFILL"),
    ("CommandSVMMap", "c'CL_COMMAND_SVM_MAP"),
    ("CommandSVMUnmap", "c'CL_COMMAND_SVM_UNMAP")
  ]
  ''CInt
  "CommandType"

convertableType
  [ ("Complete", "c'CL_COMPLETE"),
    ("Running", "c'CL_RUNNING"),
    ("Submitted", "c'CL_SUBMITTED"),
    ("Queued", "c'CL_QUEUED")
  ]
  ''CInt
  "CommandStatus"

convertableType
  [ ("BufferCreateTypeRegion", "c'CL_BUFFER_CREATE_TYPE_REGION")
  ]
  ''CInt
  "BufferCreateType"

convertableType
  [ ("ProfilingQueued", "c'CL_PROFILING_COMMAND_QUEUED"),
    ("ProfilingSubmit", "c'CL_PROFILING_COMMAND_SUBMIT"),
    ("ProfilingStart", "c'CL_PROFILING_COMMAND_START"),
    ("ProfilingEnd", "c'CL_PROFILING_COMMAND_END"),
    ("ProfilingComplete", "c'CL_PROFILING_COMMAND_COMPLETE")
  ]
  ''CInt
  "ProfilingInfo"
