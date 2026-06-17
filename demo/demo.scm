;;;============================================================================

;;; File: "test.scm"

;;; Copyright (c) 2026 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(import (github.com/gambit/llvm))

;;;----------------------------------------------------------------------------

(define (build-fib-module)

  (LLVMInitializeNativeTarget)
  (LLVMInitializeNativeAsmPrinter)

  (let* ((ctx
          (LLVMContextCreate))
         (options
          (LLVMCreatePassBuilderOptions))
         (mod
          (LLVMModuleCreateWithNameInContext "fib_module" ctx))
         (i32
          (LLVMIntTypeInContext ctx 32))
         (param-types
          (LLVMTypeRef-array i32))
         (fib-type
          (LLVMFunctionType i32 param-types 1 0))
         (fib
          (LLVMAddFunction mod "fib" fib-type)))

    (LLVMSetFunctionCallConv fib LLVMCCallConv)

    (let* ((n
            (LLVMGetParam fib 0))
           (entry
            (LLVMAppendBasicBlockInContext ctx fib "entry"))
           (base
            (LLVMAppendBasicBlockInContext ctx fib "base"))
           (recur
            (LLVMAppendBasicBlockInContext ctx fib "recur"))
           (end
            (LLVMAppendBasicBlockInContext ctx fib "end"))
           (builder
            (LLVMCreateBuilderInContext ctx)))

      ;; entry

      (LLVMPositionBuilderAtEnd builder entry)

      (let* ((one
              (LLVMConstInt i32 1 0))
             (test
              (LLVMBuildICmp builder LLVMIntSLE n one "n<=1")))
        (LLVMBuildCondBr builder test base recur))

      ;; base

      (LLVMPositionBuilderAtEnd builder base)

      (LLVMBuildBr builder end)

      ;; recur

      (LLVMPositionBuilderAtEnd builder recur)

      (let* ((n1
              (LLVMBuildSub builder n (LLVMConstInt i32 1 0) "n-1"))
             (n2
              (LLVMBuildSub builder n (LLVMConstInt i32 2 0) "n-2"))
             (call1
              (LLVMBuildCall2 builder fib-type fib (LLVMValueRef-array n1) 1 "fib(n-1)"))
             (call2
              (LLVMBuildCall2 builder fib-type fib (LLVMValueRef-array n2) 1 "fib(n-2)"))
             (sum
              (LLVMBuildAdd builder call1 call2 "sum")))

        (LLVMBuildBr builder end)

        ;; phi

        (LLVMPositionBuilderAtEnd builder end)

        (let ((result
               (LLVMBuildPhi builder i32 "result")))

          (LLVMAddIncoming result
                           (LLVMValueRef-array n sum)
                           (LLVMBasicBlockRef-array base recur)
                           2)

          (LLVMBuildRet builder result)))

      (LLVMRunPasses mod "default<O3>" #f options)

      (LLVMDumpModule mod)

      ;; Generate assembly code
      (let* ((triple
              (LLVMGetDefaultTargetTriple))
             (target
              (_LLVMGetTargetFromTriple triple))
             (tm
              (LLVMCreateTargetMachine
               target
               triple
               "generic"
               ""
               LLVMCodeGenLevelDefault
               LLVMRelocDefault
               LLVMCodeModelDefault)))
        (_LLVMTargetMachineEmitToFile
         tm
         mod
         "fib.s"
         LLVMAssemblyFile)

        (display "Generated fib.s")
        (newline)
        ))))

(display "*** Using LLVM version ")
(display (LLVMGetVersion-as-string))
(newline)

(build-fib-module)

;;;============================================================================
