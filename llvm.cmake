set(PSYC_FIND_EXTERNAL_LLVM OFF CACHE BOOL "Enable this to locate an existing LLVM on your machine via `find_package`. If disabled, llvm will be built from scratch as a submodule.")

set(LLVM_TARGETS_TO_BUILD "X86")

if(PSYC_FIND_EXTERNAL_LLVM)
	message(STATUS "Looking for external LLVM...")
	find_package(LLVM REQUIRED CONFIG)
else()
	message(STATUS "Using LLVM from within submodule.")
	if(WIN32)
		set(LLVM_HOST_TRIPLE "x86_64-pc-windows-msvc")
		set(LLVM_DEFAULT_TARGET_TRIPLE ${LLVM_HOST_TRIPLE})
	endif()
	add_subdirectory(llvm/llvm)
	set(LLVM_INCLUDE_DIRS "llvm/llvm/include" "${CMAKE_BINARY_DIR}/llvm/llvm/include/")
endif()

llvm_map_components_to_libnames(llvm_dependent_libs support core irreader executionengine
	# need this for jit compilation (which is used to parse & jit-compile build meta-regions)
	interpreter jitlink mcjit
	#codegen bits:
	#AMDGPUCodeGen AArch64CodeGen ARMCodeGen AVRCodeGen BPFCodeGen HexagonCodeGen LanaiCodeGen LoongArchCodeGen MipsCodeGen MSP430CodeGen NVPTXCodeGen PowerPCCodeGen RISCVCodeGen SparcCodeGen SystemZCodeGen VECodeGen WebAssemblyCodeGen XCoreCodeGen
	X86CodeGen X86AsmParser
	passes
	)
separate_arguments(llvm_definitions_list NATIVE_COMMAND ${LLVM_DEFINITIONS})
