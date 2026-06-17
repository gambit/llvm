;;;============================================================================

;;; File: "llvm.scm"

;;; Copyright (c) 2026 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(##supply-module github.com/gambit/llvm)

(##namespace ("github.com/gambit/llvm#"))
(##include "~~lib/gambit/prim/prim#.scm") ;; map fx+ to ##fx+, etc
(##include "~~lib/_gambit#.scm")          ;; for macro-check-procedure, 
(##include "llvm#.scm")

(define-macro (find-llvm-lib)

  (define (gen-meta-info cc-options ld-options)
    `(begin
       (##meta-info
        cc-options
        ,(string-append cc-options " -Wno-discarded-qualifiers"))
       (##meta-info
        ld-options
        ,(string-append ld-options " -lLLVM-C"))))

  (define (missing-llvm)
    (display "*** The Gambit llvm library can't be built because the llvm-config\n")
    (display "*** program could not be executed. This is usually because the LLVM\n")
    (display "*** developper package is not installed on this machine.\n")
    (exit 1))

  (define (llvm-config option)

    (define llvm-config-names
      '("llvm-config-25"
        "llvm-config-24"
        "llvm-config-23"
        "llvm-config-22"
        "llvm-config-21"
        "llvm-config-20"
        "llvm-config"))

    (let loop ((names llvm-config-names))
      (if (pair? names)
          (let* ((name
                  (car names))
                 (llvm-config-program
                  (let ((dir
                         (latest-versionned-subdir-in
                          '("/opt/homebrew/Cellar/llvm"))))
                    (if dir
                        (path-expand name (path-expand "bin" dir))
                        name)))
                 (status
                  (shell-command (string-append llvm-config-program " " option) #t)))
            (if (= 0 (car status))
                (with-input-from-string (cdr status) read-line)
                (loop (cdr names))))
          (missing-llvm))))

  (define (latest-versionned-subdir-in dirs)
    (let loop ((dirs dirs))
      (and (pair? dirs)
           (let* ((dir
                   (car dirs))
                  (subdir
                   (with-exception-catcher
                    (lambda (e)
                      #f)
                    (lambda ()
                      (let ((subdirs
                             (list-sort string>=? (directory-files dir))))
                        (and (pair? subdirs)
                             (path-expand (car subdirs) dir)))))))
             (or subdir
                 (loop (cdr dirs)))))))

  (define cc-options (llvm-config "--cflags"))
  (define ld-options (llvm-config "--ldflags"))

  (gen-meta-info cc-options ld-options))

(find-llvm-lib)

;;;============================================================================

;; For each basic C type, define the Scheme procedures:
;;
;;  (c-array-of-<type>-alloc size) => array-of-<type>
;;  (c-array-of-<type>-free array-of-<type>)
;;  (c-array-of-<type>-get array-of-<type> index) => <type>
;;  (c-array-of-<type>-set array-of-<type> index <type>)

(define-macro (define-c-array-of c-type-scheme #!optional c-type c-type-name)

  (define (sym . lst)
    (string->symbol
     (apply string-append
            (map (lambda (s) (if (symbol? s) (symbol->string s) s))
                 lst))))

  (if (not c-type)
      (set! c-type (symbol->string c-type-scheme)))

  (if (not c-type-name)
      (set! c-type-name c-type))

  `(begin

     (c-declare ,(string-append "

#include <stdlib.h>

" c-type " *c_array_of_" c-type-name "_alloc(int nb_elems) {
  " c-type " *ptr = (" c-type "*)malloc(nb_elems * sizeof(" c-type "));
  return ptr;
}

void c_array_of_" c-type-name "_free(" c-type " *array) {
  free(array);
}

" c-type " c_array_of_" c-type-name "_get(" c-type " *array, int index) {
  return array[index];
}

void c_array_of_" c-type-name "_set(" c-type " *array, int index, " c-type " value) {
  array[index] = value;
}

"))

     (define ,(sym 'c-array-of- c-type-scheme '-alloc)
       (c-lambda (int)
                 (pointer ,c-type-scheme)
                 ,(string-append "c_array_of_" c-type-name "_alloc")))

     (define ,(sym 'c-array-of- c-type-scheme '-free)
       (c-lambda ((pointer ,c-type-scheme))
                 void
                 ,(string-append "c_array_of_" c-type-name "_free")))

     (define ,(sym 'c-array-of- c-type-scheme '-get)
       (c-lambda ((pointer ,c-type-scheme) int)
                 ,c-type-scheme
                 ,(string-append "c_array_of_" c-type-name "_get")))

     (define ,(sym 'c-array-of- c-type-scheme '-set)
       (c-lambda ((pointer ,c-type-scheme) int ,c-type-scheme)
                 void
                 ,(string-append "c_array_of_" c-type-name "_set")))))

;(define-c-array-of char)
;(define-c-array-of unsigned-char "unsigned char" "unsigned_char")

;(define-c-array-of short)
;(define-c-array-of unsigned-short "unsigned short" "unsigned_short")

(define-c-array-of int)
(define-c-array-of unsigned-int "unsigned int" "unsigned_int")

;(define-c-array-of long)
;(define-c-array-of unsigned-long "unsigned long" "unsigned_long")

;(define-c-array-of long-long "long long" "long_long")
;(define-c-array-of unsigned-long-long "unsigned long long" "unsigned_long_long")

;(define-c-array-of float)
;(define-c-array-of double)

;(define-c-array-of size_t)
;(define-c-array-of ssize_t)

;;;----------------------------------------------------------------------------

;;; Interface to LLVM C API.

(c-declare #<<end-of-c-declare

#include <llvm-c/Core.h>
#include <llvm-c/Target.h>
#include <llvm-c/TargetMachine.h>
#include <llvm-c/Transforms/PassBuilder.h>

end-of-c-declare
)

(define-macro (define-c-function name params result)

  (define ignore '(

;; LLVM functions that use the global context are deprecated:
LLVMGetGlobalContext
LLVMGetMDKindID
LLVMModuleCreateWithName
LLVMInt1Type
LLVMInt8Type
LLVMInt16Type
LLVMInt32Type
LLVMInt64Type
LLVMInt128Type
LLVMIntType
LLVMHalfType
LLVMBFloatType
LLVMFloatType
LLVMDoubleType
LLVMX86FP80Type
LLVMFP128Type
LLVMPPCFP128Type
LLVMStructType
LLVMVoidType
LLVMLabelType
LLVMX86AMXType
LLVMConstString
LLVMConstStruct
LLVMMDString
LLVMMDNode
LLVMAppendBasicBlock
LLVMInsertBasicBlock
LLVMCreateBuilder
LLVMIntPtrType
LLVMIntPtrTypeForAS

;; Deprecated:
LLVMGetElementAsConstant
LLVMConstNUWNeg
LLVMBuildNUWNeg

;; Unsupported:
LLVMInstallFatalErrorHandler
LLVMContextSetDiagnosticHandler
LLVMContextGetDiagnosticHandler
LLVMContextGetDiagnosticContext
LLVMContextSetYieldCallback

))

  (if (member name ignore)
      `(begin)
      `(define ,name
         (c-lambda ,params ,result ,(symbol->string name)))))

(define-macro (define-c-enum-type name . enums)
  `(begin
     (c-define-type ,name int)
     ,@(map (lambda (x)
              `(define ,(car x) ,(cadr x)))
            enums)))

(define-macro (define-c-type name type)
  `(c-define-type ,name ,type))

;;;----------------------------------------------------------------------------

;;; Core.h

(define-c-type LLVMMemoryBufferRef (pointer (struct "LLVMOpaqueMemoryBuffer")))
(define-c-type LLVMContextRef (pointer (struct "LLVMOpaqueContext")))
(define-c-type LLVMModuleRef (pointer (struct "LLVMOpaqueModule")))
(define-c-type LLVMTypeRef (pointer (struct "LLVMOpaqueType")))
(define-c-type LLVMValueRef (pointer (struct "LLVMOpaqueValue")))
(define-c-type LLVMBasicBlockRef (pointer (struct "LLVMOpaqueBasicBlock")))
(define-c-type LLVMMetadataRef (pointer (struct "LLVMOpaqueMetadata")))
(define-c-type LLVMNamedMDNodeRef (pointer (struct "LLVMOpaqueNamedMDNode")))
(define-c-type LLVMValueMetadataEntry (struct "LLVMOpaqueValueMetadataEntry"))
(define-c-type LLVMBuilderRef (pointer (struct "LLVMOpaqueBuilder")))
(define-c-type LLVMDIBuilderRef (pointer (struct "LLVMOpaqueDIBuilder")))
(define-c-type LLVMModuleProviderRef (pointer (struct "LLVMOpaqueModuleProvider")))
(define-c-type LLVMPassManagerRef (pointer (struct "LLVMOpaquePassManager")))
(define-c-type LLVMUseRef (pointer (struct "LLVMOpaqueUse")))
(define-c-type LLVMOperandBundleRef (pointer (struct "LLVMOpaqueOperandBundle")))
(define-c-type LLVMAttributeRef (pointer (struct "LLVMOpaqueAttributeRef")))
(define-c-type LLVMDiagnosticInfoRef (pointer (struct "LLVMOpaqueDiagnosticInfo")))
(define-c-type LLVMComdatRef (pointer (struct "LLVMComdat")))
(define-c-type LLVMModuleFlagEntry (struct "LLVMOpaqueModuleFlagEntry"))
(define-c-type LLVMJITEventListenerRef (pointer (struct "LLVMOpaqueJITEventListener")))
(define-c-type LLVMBinaryRef (pointer (struct "LLVMOpaqueBinary")))
(define-c-type LLVMDbgRecordRef (pointer (struct "LLVMOpaqueDbgRecord")))


(define-c-function LLVMInstallFatalErrorHandler (LLVMFatalErrorHandler) void)

(define-c-function LLVMResetFatalErrorHandler () void)

(define-c-function LLVMEnablePrettyStackTrace () void)

(define-c-type LLVMBool bool)

(define-c-enum-type LLVMOpcode
 (LLVMRet 1)
 (LLVMBr 2)
 (LLVMSwitch 3)
 (LLVMIndirectBr 4)
 (LLVMInvoke 5)
 (LLVMUnreachable 7)
 (LLVMCallBr 67)
 (LLVMFNeg 66)
 (LLVMAdd 8)
 (LLVMFAdd 9)
 (LLVMSub 10)
 (LLVMFSub 11)
 (LLVMMul 12)
 (LLVMFMul 13)
 (LLVMUDiv 14)
 (LLVMSDiv 15)
 (LLVMFDiv 16)
 (LLVMURem 17)
 (LLVMSRem 18)
 (LLVMFRem 19)
 (LLVMShl 20)
 (LLVMLShr 21)
 (LLVMAShr 22)
 (LLVMAnd 23)
 (LLVMOr 24)
 (LLVMXor 25)
 (LLVMAlloca 26)
 (LLVMLoad 27)
 (LLVMStore 28)
 (LLVMGetElementPtr 29)
 (LLVMTrunc 30)
 (LLVMZExt 31)
 (LLVMSExt 32)
 (LLVMFPToUI 33)
 (LLVMFPToSI 34)
 (LLVMUIToFP 35)
 (LLVMSIToFP 36)
 (LLVMFPTrunc 37)
 (LLVMFPExt 38)
 (LLVMPtrToInt 39)
 (LLVMPtrToAddr 69)
 (LLVMIntToPtr 40)
 (LLVMBitCast 41)
 (LLVMAddrSpaceCast 60)
 (LLVMICmp 42)
 (LLVMFCmp 43)
 (LLVMPHI 44)
 (LLVMCall 45)
 (LLVMSelect 46)
 (LLVMUserOp1 47)
 (LLVMUserOp2 48)
 (LLVMVAArg 49)
 (LLVMExtractElement 50)
 (LLVMInsertElement 51)
 (LLVMShuffleVector 52)
 (LLVMExtractValue 53)
 (LLVMInsertValue 54)
 (LLVMFreeze 68)
 (LLVMFence 55)
 (LLVMAtomicCmpXchg 56)
 (LLVMAtomicRMW 57)
 (LLVMResume 58)
 (LLVMLandingPad 59)
 (LLVMCleanupRet 61)
 (LLVMCatchRet 62)
 (LLVMCatchPad 63)
 (LLVMCleanupPad 64)
 (LLVMCatchSwitch 65))

(define-c-enum-type LLVMTypeKind
 (LLVMVoidTypeKind 0)
 (LLVMHalfTypeKind 1)
 (LLVMFloatTypeKind 2)
 (LLVMDoubleTypeKind 3)
 (LLVMX86_FP80TypeKind 4)
 (LLVMFP128TypeKind 5)
 (LLVMPPC_FP128TypeKind 6)
 (LLVMLabelTypeKind 7)
 (LLVMIntegerTypeKind 8)
 (LLVMFunctionTypeKind 9)
 (LLVMStructTypeKind 10)
 (LLVMArrayTypeKind 11)
 (LLVMPointerTypeKind 12)
 (LLVMVectorTypeKind 13)
 (LLVMMetadataTypeKind 14)
 (LLVMTokenTypeKind 16)
 (LLVMScalableVectorTypeKind 17)
 (LLVMBFloatTypeKind 18)
 (LLVMX86_AMXTypeKind 19)
 (LLVMTargetExtTypeKind 20))

(define-c-enum-type LLVMLinkage
 (LLVMExternalLinkage 0)
 (LLVMLinkOnceAnyLinkage 1)
 (LLVMLinkOnceODRLinkage 2)
 (LLVMLinkOnceODRAutoHideLinkage 3)
 (LLVMWeakAnyLinkage 4)
 (LLVMWeakODRLinkage 5)
 (LLVMAppendingLinkage 6)
 (LLVMInternalLinkage 7)
 (LLVMPrivateLinkage 8)
 (LLVMDLLImportLinkage 9)
 (LLVMDLLExportLinkage 10)
 (LLVMExternalWeakLinkage 11)
 (LLVMGhostLinkage 12)
 (LLVMCommonLinkage 13)
 (LLVMLinkerPrivateLinkage 14)
 (LLVMLinkerPrivateWeakLinkage 15))

(define-c-enum-type LLVMVisibility
 (LLVMDefaultVisibility 0)
 (LLVMHiddenVisibility 1)
 (LLVMProtectedVisibility 2))

(define-c-enum-type LLVMUnnamedAddr
 (LLVMNoUnnamedAddr 0)
 (LLVMLocalUnnamedAddr 1)
 (LLVMGlobalUnnamedAddr 2))

(define-c-enum-type LLVMDLLStorageClass
 (LLVMDefaultStorageClass 0)
 (LLVMDLLImportStorageClass 1)
 (LLVMDLLExportStorageClass 2))

(define-c-enum-type LLVMCallConv
 (LLVMCCallConv 0)
 (LLVMFastCallConv 8)
 (LLVMColdCallConv 9)
 (LLVMGHCCallConv 10)
 (LLVMHiPECallConv 11)
 (LLVMAnyRegCallConv 13)
 (LLVMPreserveMostCallConv 14)
 (LLVMPreserveAllCallConv 15)
 (LLVMSwiftCallConv 16)
 (LLVMCXXFASTTLSCallConv 17)
 (LLVMX86StdcallCallConv 64)
 (LLVMX86FastcallCallConv 65)
 (LLVMARMAPCSCallConv 66)
 (LLVMARMAAPCSCallConv 67)
 (LLVMARMAAPCSVFPCallConv 68)
 (LLVMMSP430INTRCallConv 69)
 (LLVMX86ThisCallCallConv 70)
 (LLVMPTXKernelCallConv 71)
 (LLVMPTXDeviceCallConv 72)
 (LLVMSPIRFUNCCallConv 75)
 (LLVMSPIRKERNELCallConv 76)
 (LLVMIntelOCLBICallConv 77)
 (LLVMX8664SysVCallConv 78)
 (LLVMWin64CallConv 79)
 (LLVMX86VectorCallCallConv 80)
 (LLVMHHVMCallConv 81)
 (LLVMHHVMCCallConv 82)
 (LLVMX86INTRCallConv 83)
 (LLVMAVRINTRCallConv 84)
 (LLVMAVRSIGNALCallConv 85)
 (LLVMAVRBUILTINCallConv 86)
 (LLVMAMDGPUVSCallConv 87)
 (LLVMAMDGPUGSCallConv 88)
 (LLVMAMDGPUPSCallConv 89)
 (LLVMAMDGPUCSCallConv 90)
 (LLVMAMDGPUKERNELCallConv 91)
 (LLVMX86RegCallCallConv 92)
 (LLVMAMDGPUHSCallConv 93)
 (LLVMMSP430BUILTINCallConv 94)
 (LLVMAMDGPULSCallConv 95)
 (LLVMAMDGPUESCallConv 96))

(define-c-enum-type  LLVMValueKind)

(define-c-enum-type LLVMIntPredicate
 (LLVMIntEQ 32)
 (LLVMIntNE 33)
 (LLVMIntUGT 34)
 (LLVMIntUGE 35)
 (LLVMIntULT 36)
 (LLVMIntULE 37)
 (LLVMIntSGT 38)
 (LLVMIntSGE 39)
 (LLVMIntSLT 40)
 (LLVMIntSLE 41))

(define-c-enum-type LLVMRealPredicate
 (LLVMRealPredicateFalse 0)
 (LLVMRealOEQ 1)
 (LLVMRealOGT 2)
 (LLVMRealOGE 3)
 (LLVMRealOLT 4)
 (LLVMRealOLE 5)
 (LLVMRealONE 6)
 (LLVMRealORD 7)
 (LLVMRealUNO 8)
 (LLVMRealUEQ 9)
 (LLVMRealUGT 10)
 (LLVMRealUGE 11)
 (LLVMRealULT 12)
 (LLVMRealULE 13)
 (LLVMRealUNE 14)
 (LLVMRealPredicateTrue 15))

(define-c-enum-type LLVMThreadLocalMode
 (LLVMNotThreadLocal 0))

(define-c-enum-type LLVMAtomicOrdering
 (LLVMAtomicOrderingNotAtomic 0)
 (LLVMAtomicOrderingUnordered 1)
 (LLVMAtomicOrderingMonotonic 2)
 (LLVMAtomicOrderingAcquire 4)
 (LLVMAtomicOrderingRelease 5)
 (LLVMAtomicOrderingAcquireRelease 6)
 (LLVMAtomicOrderingSequentiallyConsistent 7))

(define-c-enum-type LLVMAtomicRMWBinOp
 (LLVMAtomicRMWBinOpXchg 0)
 (LLVMAtomicRMWBinOpAdd 1)
 (LLVMAtomicRMWBinOpSub 2)
 (LLVMAtomicRMWBinOpAnd 3)
 (LLVMAtomicRMWBinOpNand 4)
 (LLVMAtomicRMWBinOpOr 5)
 (LLVMAtomicRMWBinOpXor 6)
 (LLVMAtomicRMWBinOpMax 7)
 (LLVMAtomicRMWBinOpMin 8)
 (LLVMAtomicRMWBinOpUMax 9)
 (LLVMAtomicRMWBinOpUMin 10)
 (LLVMAtomicRMWBinOpFAdd 11)
 (LLVMAtomicRMWBinOpFSub 12)
 (LLVMAtomicRMWBinOpFMax 13)
 (LLVMAtomicRMWBinOpFMin 14)
 (LLVMAtomicRMWBinOpUIncWrap 15)
 (LLVMAtomicRMWBinOpUDecWrap 16)
 (LLVMAtomicRMWBinOpUSubCond 17)
 (LLVMAtomicRMWBinOpUSubSat 18)
 (LLVMAtomicRMWBinOpFMaximum 19)
 (LLVMAtomicRMWBinOpFMinimum 20))

(define-c-enum-type LLVMDiagnosticSeverity)

(define-c-enum-type LLVMInlineAsmDialect)

(define-c-enum-type LLVMModuleFlagBehavior
 (LLVMModuleFlagBehaviorError 0)
 (LLVMModuleFlagBehaviorWarning 1)
 (LLVMModuleFlagBehaviorRequire 2)
 (LLVMModuleFlagBehaviorOverride 3)
 (LLVMModuleFlagBehaviorAppend 4)
 (LLVMModuleFlagBehaviorAppendUnique 5))

(define-c-type LLVMAttributeIndex unsigned-int)

(define-c-enum-type LLVMTailCallKind
 (LLVMTailCallKindNone 0)
 (LLVMTailCallKindTail 1)
 (LLVMTailCallKindMustTail 2)
 (LLVMTailCallKindNoTail 3))

(define-c-type LLVMFastMathFlags unsigned-int)

(define-c-type LLVMGEPNoWrapFlags unsigned-int)

(define-c-enum-type LLVMDbgRecordKind)

(define-c-function LLVMShutdown () void)

(define-c-function
 LLVMGetVersion
 ((pointer unsigned-int) (pointer unsigned-int) (pointer unsigned-int))
 void)

(define-c-function LLVMCreateMessage (char-string) char-string)

(define-c-function LLVMDisposeMessage (char-string) void)

(define-c-function LLVMContextCreate () LLVMContextRef)

(define-c-function LLVMGetGlobalContext () LLVMContextRef)

(define-c-function
 LLVMContextSetDiagnosticHandler
 (LLVMContextRef LLVMDiagnosticHandler (pointer void))
 void)

(define-c-function
 LLVMContextGetDiagnosticHandler
 (LLVMContextRef)
 LLVMDiagnosticHandler)

(define-c-function
 LLVMContextGetDiagnosticContext
 (LLVMContextRef)
 (pointer void))

(define-c-function
 LLVMContextSetYieldCallback
 (LLVMContextRef LLVMYieldCallback (pointer void))
 void)

(define-c-function
 LLVMContextShouldDiscardValueNames
 (LLVMContextRef)
 LLVMBool)

(define-c-function
 LLVMContextSetDiscardValueNames
 (LLVMContextRef LLVMBool)
 void)

(define-c-function LLVMContextDispose (LLVMContextRef) void)

(define-c-function
 LLVMGetDiagInfoDescription
 (LLVMDiagnosticInfoRef)
 char-string)

(define-c-function
 LLVMGetDiagInfoSeverity
 (LLVMDiagnosticInfoRef)
 LLVMDiagnosticSeverity)

(define-c-function
 LLVMGetMDKindIDInContext
 (LLVMContextRef char-string unsigned-int)
 unsigned-int)

(define-c-function LLVMGetMDKindID (char-string unsigned-int) unsigned-int)

(define-c-function
 LLVMGetSyncScopeID
 (LLVMContextRef char-string size_t)
 unsigned-int)

(define-c-function
 LLVMGetEnumAttributeKindForName
 (char-string size_t)
 unsigned-int)

(define-c-function LLVMGetLastEnumAttributeKind () unsigned-int)

(define-c-function
 LLVMCreateEnumAttribute
 (LLVMContextRef unsigned-int unsigned-int64)
 LLVMAttributeRef)

(define-c-function LLVMGetEnumAttributeKind (LLVMAttributeRef) unsigned-int)

(define-c-function LLVMGetEnumAttributeValue (LLVMAttributeRef) unsigned-int64)

(define-c-function
 LLVMCreateTypeAttribute
 (LLVMContextRef unsigned-int LLVMTypeRef)
 LLVMAttributeRef)

(define-c-function LLVMGetTypeAttributeValue (LLVMAttributeRef) LLVMTypeRef)

(define-c-function
 LLVMCreateConstantRangeAttribute
 (LLVMContextRef
  unsigned-int
  unsigned-int
  (pointer unsigned-long-long)
  (pointer unsigned-long-long))
 LLVMAttributeRef)

(define-c-function
 LLVMCreateStringAttribute
 (LLVMContextRef char-string unsigned-int char-string unsigned-int)
 LLVMAttributeRef)

(define-c-function
 LLVMGetStringAttributeKind
 (LLVMAttributeRef (pointer unsigned-int))
 char-string)

(define-c-function
 LLVMGetStringAttributeValue
 (LLVMAttributeRef (pointer unsigned-int))
 char-string)

(define-c-function LLVMIsEnumAttribute (LLVMAttributeRef) LLVMBool)

(define-c-function LLVMIsStringAttribute (LLVMAttributeRef) LLVMBool)

(define-c-function LLVMIsTypeAttribute (LLVMAttributeRef) LLVMBool)

(define-c-function LLVMGetTypeByName2 (LLVMContextRef char-string) LLVMTypeRef)

(define-c-function LLVMModuleCreateWithName (char-string) LLVMModuleRef)

(define-c-function
 LLVMModuleCreateWithNameInContext
 (char-string LLVMContextRef)
 LLVMModuleRef)

(define-c-function LLVMCloneModule (LLVMModuleRef) LLVMModuleRef)

(define-c-function LLVMDisposeModule (LLVMModuleRef) void)

(define-c-function LLVMIsNewDbgInfoFormat (LLVMModuleRef) LLVMBool)

(define-c-function LLVMSetIsNewDbgInfoFormat (LLVMModuleRef LLVMBool) void)

(define-c-function
 LLVMGetModuleIdentifier
 (LLVMModuleRef (pointer size_t))
 char-string)

(define-c-function
 LLVMSetModuleIdentifier
 (LLVMModuleRef char-string size_t)
 void)

(define-c-function
 LLVMGetSourceFileName
 (LLVMModuleRef (pointer size_t))
 char-string)

(define-c-function
 LLVMSetSourceFileName
 (LLVMModuleRef char-string size_t)
 void)

(define-c-function LLVMGetDataLayoutStr (LLVMModuleRef) char-string)

(define-c-function LLVMGetDataLayout (LLVMModuleRef) char-string)

(define-c-function LLVMSetDataLayout (LLVMModuleRef char-string) void)

(define-c-function LLVMGetTarget (LLVMModuleRef) char-string)

(define-c-function LLVMSetTarget (LLVMModuleRef char-string) void)

(define-c-function
 LLVMCopyModuleFlagsMetadata
 (LLVMModuleRef (pointer size_t))
 (pointer LLVMModuleFlagEntry))

(define-c-function
 LLVMDisposeModuleFlagsMetadata
 ((pointer LLVMModuleFlagEntry))
 void)

(define-c-function
 LLVMModuleFlagEntriesGetFlagBehavior
 ((pointer LLVMModuleFlagEntry) unsigned-int)
 LLVMModuleFlagBehavior)

(define-c-function
 LLVMModuleFlagEntriesGetKey
 ((pointer LLVMModuleFlagEntry) unsigned-int (pointer size_t))
 char-string)

(define-c-function
 LLVMModuleFlagEntriesGetMetadata
 ((pointer LLVMModuleFlagEntry) unsigned-int)
 LLVMMetadataRef)

(define-c-function
 LLVMGetModuleFlag
 (LLVMModuleRef char-string size_t)
 LLVMMetadataRef)

(define-c-function
 LLVMAddModuleFlag
 (LLVMModuleRef LLVMModuleFlagBehavior char-string size_t LLVMMetadataRef)
 void)

(define-c-function LLVMDumpModule (LLVMModuleRef) void)

(define-c-function
 LLVMPrintModuleToFile
 (LLVMModuleRef char-string (pointer char-string))
 LLVMBool)

(define-c-function LLVMPrintModuleToString (LLVMModuleRef) char-string)

(define-c-function
 LLVMGetModuleInlineAsm
 (LLVMModuleRef (pointer size_t))
 char-string)

(define-c-function
 LLVMSetModuleInlineAsm2
 (LLVMModuleRef char-string size_t)
 void)

(define-c-function
 LLVMAppendModuleInlineAsm
 (LLVMModuleRef char-string size_t)
 void)

(define-c-function
 LLVMGetInlineAsm
 (LLVMTypeRef
  char-string
  size_t
  char-string
  size_t
  LLVMBool
  LLVMBool
  LLVMInlineAsmDialect
  LLVMBool)
 LLVMValueRef)

(define-c-function
 LLVMGetInlineAsmAsmString
 (LLVMValueRef (pointer size_t))
 char-string)

(define-c-function
 LLVMGetInlineAsmConstraintString
 (LLVMValueRef (pointer size_t))
 char-string)

(define-c-function LLVMGetInlineAsmDialect (LLVMValueRef) LLVMInlineAsmDialect)

(define-c-function LLVMGetInlineAsmFunctionType (LLVMValueRef) LLVMTypeRef)

(define-c-function LLVMGetInlineAsmHasSideEffects (LLVMValueRef) LLVMBool)

(define-c-function LLVMGetInlineAsmNeedsAlignedStack (LLVMValueRef) LLVMBool)

(define-c-function LLVMGetInlineAsmCanUnwind (LLVMValueRef) LLVMBool)

(define-c-function LLVMGetModuleContext (LLVMModuleRef) LLVMContextRef)

(define-c-function LLVMGetTypeByName (LLVMModuleRef char-string) LLVMTypeRef)

(define-c-function
 LLVMGetFirstNamedMetadata
 (LLVMModuleRef)
 LLVMNamedMDNodeRef)

(define-c-function LLVMGetLastNamedMetadata (LLVMModuleRef) LLVMNamedMDNodeRef)

(define-c-function
 LLVMGetNextNamedMetadata
 (LLVMNamedMDNodeRef)
 LLVMNamedMDNodeRef)

(define-c-function
 LLVMGetPreviousNamedMetadata
 (LLVMNamedMDNodeRef)
 LLVMNamedMDNodeRef)

(define-c-function
 LLVMGetNamedMetadata
 (LLVMModuleRef char-string size_t)
 LLVMNamedMDNodeRef)

(define-c-function
 LLVMGetOrInsertNamedMetadata
 (LLVMModuleRef char-string size_t)
 LLVMNamedMDNodeRef)

(define-c-function
 LLVMGetNamedMetadataName
 (LLVMNamedMDNodeRef (pointer size_t))
 char-string)

(define-c-function
 LLVMGetNamedMetadataNumOperands
 (LLVMModuleRef char-string)
 unsigned-int)

(define-c-function
 LLVMGetNamedMetadataOperands
 (LLVMModuleRef char-string (pointer LLVMValueRef))
 void)

(define-c-function
 LLVMAddNamedMetadataOperand
 (LLVMModuleRef char-string LLVMValueRef)
 void)

(define-c-function
 LLVMGetDebugLocDirectory
 (LLVMValueRef (pointer unsigned-int))
 char-string)

(define-c-function
 LLVMGetDebugLocFilename
 (LLVMValueRef (pointer unsigned-int))
 char-string)

(define-c-function LLVMGetDebugLocLine (LLVMValueRef) unsigned-int)

(define-c-function LLVMGetDebugLocColumn (LLVMValueRef) unsigned-int)

(define-c-function
 LLVMAddFunction
 (LLVMModuleRef char-string LLVMTypeRef)
 LLVMValueRef)

(define-c-function
 LLVMGetOrInsertFunction
 (LLVMModuleRef char-string size_t LLVMTypeRef)
 LLVMValueRef)

(define-c-function
 LLVMGetNamedFunction
 (LLVMModuleRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMGetNamedFunctionWithLength
 (LLVMModuleRef char-string size_t)
 LLVMValueRef)

(define-c-function LLVMGetFirstFunction (LLVMModuleRef) LLVMValueRef)

(define-c-function LLVMGetLastFunction (LLVMModuleRef) LLVMValueRef)

(define-c-function LLVMGetNextFunction (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetPreviousFunction (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMSetModuleInlineAsm (LLVMModuleRef char-string) void)

(define-c-function LLVMGetTypeKind (LLVMTypeRef) LLVMTypeKind)

(define-c-function LLVMTypeIsSized (LLVMTypeRef) LLVMBool)

(define-c-function LLVMGetTypeContext (LLVMTypeRef) LLVMContextRef)

(define-c-function LLVMDumpType (LLVMTypeRef) void)

(define-c-function LLVMPrintTypeToString (LLVMTypeRef) char-string)

(define-c-function LLVMInt1TypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMInt8TypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMInt16TypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMInt32TypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMInt64TypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMInt128TypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function
 LLVMIntTypeInContext
 (LLVMContextRef unsigned-int)
 LLVMTypeRef)

(define-c-function LLVMInt1Type () LLVMTypeRef)

(define-c-function LLVMInt8Type () LLVMTypeRef)

(define-c-function LLVMInt16Type () LLVMTypeRef)

(define-c-function LLVMInt32Type () LLVMTypeRef)

(define-c-function LLVMInt64Type () LLVMTypeRef)

(define-c-function LLVMInt128Type () LLVMTypeRef)

(define-c-function LLVMIntType (unsigned-int) LLVMTypeRef)

(define-c-function LLVMGetIntTypeWidth (LLVMTypeRef) unsigned-int)

(define-c-function LLVMHalfTypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMBFloatTypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMFloatTypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMDoubleTypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMX86FP80TypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMFP128TypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMPPCFP128TypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMHalfType () LLVMTypeRef)

(define-c-function LLVMBFloatType () LLVMTypeRef)

(define-c-function LLVMFloatType () LLVMTypeRef)

(define-c-function LLVMDoubleType () LLVMTypeRef)

(define-c-function LLVMX86FP80Type () LLVMTypeRef)

(define-c-function LLVMFP128Type () LLVMTypeRef)

(define-c-function LLVMPPCFP128Type () LLVMTypeRef)

(define-c-function
 LLVMFunctionType
 (LLVMTypeRef (pointer LLVMTypeRef) unsigned-int LLVMBool)
 LLVMTypeRef)

(define-c-function LLVMIsFunctionVarArg (LLVMTypeRef) LLVMBool)

(define-c-function LLVMGetReturnType (LLVMTypeRef) LLVMTypeRef)

(define-c-function LLVMCountParamTypes (LLVMTypeRef) unsigned-int)

(define-c-function LLVMGetParamTypes (LLVMTypeRef (pointer LLVMTypeRef)) void)

(define-c-function
 LLVMStructTypeInContext
 (LLVMContextRef (pointer LLVMTypeRef) unsigned-int LLVMBool)
 LLVMTypeRef)

(define-c-function
 LLVMStructType
 ((pointer LLVMTypeRef) unsigned-int LLVMBool)
 LLVMTypeRef)

(define-c-function
 LLVMStructCreateNamed
 (LLVMContextRef char-string)
 LLVMTypeRef)

(define-c-function LLVMGetStructName (LLVMTypeRef) char-string)

(define-c-function
 LLVMStructSetBody
 (LLVMTypeRef (pointer LLVMTypeRef) unsigned-int LLVMBool)
 void)

(define-c-function LLVMCountStructElementTypes (LLVMTypeRef) unsigned-int)

(define-c-function
 LLVMGetStructElementTypes
 (LLVMTypeRef (pointer LLVMTypeRef))
 void)

(define-c-function
 LLVMStructGetTypeAtIndex
 (LLVMTypeRef unsigned-int)
 LLVMTypeRef)

(define-c-function LLVMIsPackedStruct (LLVMTypeRef) LLVMBool)

(define-c-function LLVMIsOpaqueStruct (LLVMTypeRef) LLVMBool)

(define-c-function LLVMIsLiteralStruct (LLVMTypeRef) LLVMBool)

(define-c-function LLVMGetElementType (LLVMTypeRef) LLVMTypeRef)

(define-c-function LLVMGetSubtypes (LLVMTypeRef (pointer LLVMTypeRef)) void)

(define-c-function LLVMGetNumContainedTypes (LLVMTypeRef) unsigned-int)

(define-c-function LLVMArrayType (LLVMTypeRef unsigned-int) LLVMTypeRef)

(define-c-function LLVMArrayType2 (LLVMTypeRef unsigned-int64) LLVMTypeRef)

(define-c-function LLVMGetArrayLength (LLVMTypeRef) unsigned-int)

(define-c-function LLVMGetArrayLength2 (LLVMTypeRef) unsigned-int64)

(define-c-function LLVMPointerType (LLVMTypeRef unsigned-int) LLVMTypeRef)

(define-c-function LLVMPointerTypeIsOpaque (LLVMTypeRef) LLVMBool)

(define-c-function
 LLVMPointerTypeInContext
 (LLVMContextRef unsigned-int)
 LLVMTypeRef)

(define-c-function LLVMGetPointerAddressSpace (LLVMTypeRef) unsigned-int)

(define-c-function LLVMVectorType (LLVMTypeRef unsigned-int) LLVMTypeRef)

(define-c-function
 LLVMScalableVectorType
 (LLVMTypeRef unsigned-int)
 LLVMTypeRef)

(define-c-function LLVMGetVectorSize (LLVMTypeRef) unsigned-int)

(define-c-function LLVMGetConstantPtrAuthPointer (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetConstantPtrAuthKey (LLVMValueRef) LLVMValueRef)

(define-c-function
 LLVMGetConstantPtrAuthDiscriminator
 (LLVMValueRef)
 LLVMValueRef)

(define-c-function
 LLVMGetConstantPtrAuthAddrDiscriminator
 (LLVMValueRef)
 LLVMValueRef)

(define-c-function LLVMVoidTypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMLabelTypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMX86AMXTypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMTokenTypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMMetadataTypeInContext (LLVMContextRef) LLVMTypeRef)

(define-c-function LLVMVoidType () LLVMTypeRef)

(define-c-function LLVMLabelType () LLVMTypeRef)

(define-c-function LLVMX86AMXType () LLVMTypeRef)

(define-c-function
 LLVMTargetExtTypeInContext
 (LLVMContextRef
  char-string
  (pointer LLVMTypeRef)
  unsigned-int
  (pointer unsigned-int)
  unsigned-int)
 LLVMTypeRef)

(define-c-function LLVMGetTargetExtTypeName (LLVMTypeRef) char-string)

(define-c-function
 LLVMGetTargetExtTypeNumTypeParams
 (LLVMTypeRef)
 unsigned-int)

(define-c-function
 LLVMGetTargetExtTypeTypeParam
 (LLVMTypeRef unsigned-int)
 LLVMTypeRef)

(define-c-function LLVMGetTargetExtTypeNumIntParams (LLVMTypeRef) unsigned-int)

(define-c-function
 LLVMGetTargetExtTypeIntParam
 (LLVMTypeRef unsigned-int)
 unsigned-int)

(define-c-function LLVMTypeOf (LLVMValueRef) LLVMTypeRef)

(define-c-function LLVMGetValueKind (LLVMValueRef) LLVMValueKind)

(define-c-function
 LLVMGetValueName2
 (LLVMValueRef (pointer size_t))
 char-string)

(define-c-function LLVMSetValueName2 (LLVMValueRef char-string size_t) void)

(define-c-function LLVMDumpValue (LLVMValueRef) void)

(define-c-function LLVMPrintValueToString (LLVMValueRef) char-string)

(define-c-function LLVMGetValueContext (LLVMValueRef) LLVMContextRef)

(define-c-function LLVMPrintDbgRecordToString (LLVMDbgRecordRef) char-string)

(define-c-function LLVMReplaceAllUsesWith (LLVMValueRef LLVMValueRef) void)

(define-c-function LLVMIsConstant (LLVMValueRef) LLVMBool)

(define-c-function LLVMIsUndef (LLVMValueRef) LLVMBool)

(define-c-function LLVMIsPoison (LLVMValueRef) LLVMBool)

(define-c-function LLVMIsAArgument (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsABasicBlock (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAInlineAsm (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAUser (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstant (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsABlockAddress (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantAggregateZero (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantArray (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantDataSequential (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantDataArray (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantDataVector (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantExpr (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantFP (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantInt (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantPointerNull (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantStruct (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantTokenNone (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantVector (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAConstantPtrAuth (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAGlobalValue (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAGlobalAlias (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAGlobalObject (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAFunction (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAGlobalVariable (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAGlobalIFunc (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAUndefValue (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAPoisonValue (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAInstruction (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAUnaryOperator (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsABinaryOperator (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsACallInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAIntrinsicInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsADbgInfoIntrinsic (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsADbgVariableIntrinsic (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsADbgDeclareInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsADbgLabelInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAMemIntrinsic (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAMemCpyInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAMemMoveInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAMemSetInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsACmpInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAFCmpInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAICmpInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAExtractElementInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAGetElementPtrInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAInsertElementInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAInsertValueInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsALandingPadInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAPHINode (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsASelectInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAShuffleVectorInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAStoreInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsABranchInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAIndirectBrInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAInvokeInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAReturnInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsASwitchInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAUnreachableInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAResumeInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsACleanupReturnInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsACatchReturnInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsACatchSwitchInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsACallBrInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAFuncletPadInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsACatchPadInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsACleanupPadInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAUnaryInstruction (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAAllocaInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsACastInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAAddrSpaceCastInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsABitCastInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAFPExtInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAFPToSIInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAFPToUIInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAFPTruncInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAIntToPtrInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAPtrToIntInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsASExtInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsASIToFPInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsATruncInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAUIToFPInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAZExtInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAExtractValueInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsALoadInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAVAArgInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAFreezeInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAAtomicCmpXchgInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAAtomicRMWInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAFenceInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAMDNode (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAValueAsMetadata (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsAMDString (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetValueName (LLVMValueRef) char-string)

(define-c-function LLVMSetValueName (LLVMValueRef char-string) void)

(define-c-function LLVMGetFirstUse (LLVMValueRef) LLVMUseRef)

(define-c-function LLVMGetNextUse (LLVMUseRef) LLVMUseRef)

(define-c-function LLVMGetUser (LLVMUseRef) LLVMValueRef)

(define-c-function LLVMGetUsedValue (LLVMUseRef) LLVMValueRef)

(define-c-function LLVMGetOperand (LLVMValueRef unsigned-int) LLVMValueRef)

(define-c-function LLVMGetOperandUse (LLVMValueRef unsigned-int) LLVMUseRef)

(define-c-function
 LLVMSetOperand
 (LLVMValueRef unsigned-int LLVMValueRef)
 void)

(define-c-function LLVMGetNumOperands (LLVMValueRef) int)

(define-c-function LLVMConstNull (LLVMTypeRef) LLVMValueRef)

(define-c-function LLVMConstAllOnes (LLVMTypeRef) LLVMValueRef)

(define-c-function LLVMGetUndef (LLVMTypeRef) LLVMValueRef)

(define-c-function LLVMGetPoison (LLVMTypeRef) LLVMValueRef)

(define-c-function LLVMIsNull (LLVMValueRef) LLVMBool)

(define-c-function LLVMConstPointerNull (LLVMTypeRef) LLVMValueRef)

(define-c-function
 LLVMConstInt
 (LLVMTypeRef unsigned-long-long LLVMBool)
 LLVMValueRef)

(define-c-function
 LLVMConstIntOfArbitraryPrecision
 (LLVMTypeRef unsigned-int (pointer unsigned-long-long))
 LLVMValueRef)

(define-c-function
 LLVMConstIntOfString
 (LLVMTypeRef char-string unsigned-int8)
 LLVMValueRef)

(define-c-function
 LLVMConstIntOfStringAndSize
 (LLVMTypeRef char-string unsigned-int unsigned-int8)
 LLVMValueRef)

(define-c-function LLVMConstReal (LLVMTypeRef double) LLVMValueRef)

(define-c-function
 LLVMConstRealOfString
 (LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMConstRealOfStringAndSize
 (LLVMTypeRef char-string unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMConstFPFromBits
 (LLVMTypeRef (pointer unsigned-long-long))
 LLVMValueRef)

(define-c-function LLVMConstIntGetZExtValue (LLVMValueRef) unsigned-long-long)

(define-c-function LLVMConstIntGetSExtValue (LLVMValueRef) long-long)

(define-c-function
 LLVMConstRealGetDouble
 (LLVMValueRef (pointer LLVMBool))
 double)

(define-c-function
 LLVMConstStringInContext
 (LLVMContextRef char-string unsigned-int LLVMBool)
 LLVMValueRef)

(define-c-function
 LLVMConstStringInContext2
 (LLVMContextRef char-string size_t LLVMBool)
 LLVMValueRef)

(define-c-function
 LLVMConstString
 (char-string unsigned-int LLVMBool)
 LLVMValueRef)

(define-c-function LLVMIsConstantString (LLVMValueRef) LLVMBool)

(define-c-function LLVMGetAsString (LLVMValueRef (pointer size_t)) char-string)

(define-c-function
 LLVMGetRawDataValues
 (LLVMValueRef (pointer size_t))
 char-string)

(define-c-function
 LLVMConstStructInContext
 (LLVMContextRef (pointer LLVMValueRef) unsigned-int LLVMBool)
 LLVMValueRef)

(define-c-function
 LLVMConstStruct
 ((pointer LLVMValueRef) unsigned-int LLVMBool)
 LLVMValueRef)

(define-c-function
 LLVMConstArray
 (LLVMTypeRef (pointer LLVMValueRef) unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMConstArray2
 (LLVMTypeRef (pointer LLVMValueRef) unsigned-int64)
 LLVMValueRef)

(define-c-function
 LLVMConstDataArray
 (LLVMTypeRef char-string size_t)
 LLVMValueRef)

(define-c-function
 LLVMConstNamedStruct
 (LLVMTypeRef (pointer LLVMValueRef) unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMGetAggregateElement
 (LLVMValueRef unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMGetElementAsConstant
 (LLVMValueRef unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMConstVector
 ((pointer LLVMValueRef) unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMConstantPtrAuth
 (LLVMValueRef LLVMValueRef LLVMValueRef LLVMValueRef)
 LLVMValueRef)

(define-c-function LLVMGetConstOpcode (LLVMValueRef) LLVMOpcode)

(define-c-function LLVMAlignOf (LLVMTypeRef) LLVMValueRef)

(define-c-function LLVMSizeOf (LLVMTypeRef) LLVMValueRef)

(define-c-function LLVMConstNeg (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMConstNSWNeg (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMConstNUWNeg (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMConstNot (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMConstAdd (LLVMValueRef LLVMValueRef) LLVMValueRef)

(define-c-function LLVMConstNSWAdd (LLVMValueRef LLVMValueRef) LLVMValueRef)

(define-c-function LLVMConstNUWAdd (LLVMValueRef LLVMValueRef) LLVMValueRef)

(define-c-function LLVMConstSub (LLVMValueRef LLVMValueRef) LLVMValueRef)

(define-c-function LLVMConstNSWSub (LLVMValueRef LLVMValueRef) LLVMValueRef)

(define-c-function LLVMConstNUWSub (LLVMValueRef LLVMValueRef) LLVMValueRef)

(define-c-function LLVMConstXor (LLVMValueRef LLVMValueRef) LLVMValueRef)

(define-c-function
 LLVMConstGEP2
 (LLVMTypeRef LLVMValueRef (pointer LLVMValueRef) unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMConstInBoundsGEP2
 (LLVMTypeRef LLVMValueRef (pointer LLVMValueRef) unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMConstGEPWithNoWrapFlags
 (LLVMTypeRef
  LLVMValueRef
  (pointer LLVMValueRef)
  unsigned-int
  LLVMGEPNoWrapFlags)
 LLVMValueRef)

(define-c-function LLVMConstTrunc (LLVMValueRef LLVMTypeRef) LLVMValueRef)

(define-c-function LLVMConstPtrToInt (LLVMValueRef LLVMTypeRef) LLVMValueRef)

(define-c-function LLVMConstIntToPtr (LLVMValueRef LLVMTypeRef) LLVMValueRef)

(define-c-function LLVMConstBitCast (LLVMValueRef LLVMTypeRef) LLVMValueRef)

(define-c-function
 LLVMConstAddrSpaceCast
 (LLVMValueRef LLVMTypeRef)
 LLVMValueRef)

(define-c-function
 LLVMConstTruncOrBitCast
 (LLVMValueRef LLVMTypeRef)
 LLVMValueRef)

(define-c-function
 LLVMConstPointerCast
 (LLVMValueRef LLVMTypeRef)
 LLVMValueRef)

(define-c-function
 LLVMConstExtractElement
 (LLVMValueRef LLVMValueRef)
 LLVMValueRef)

(define-c-function
 LLVMConstInsertElement
 (LLVMValueRef LLVMValueRef LLVMValueRef)
 LLVMValueRef)

(define-c-function
 LLVMConstShuffleVector
 (LLVMValueRef LLVMValueRef LLVMValueRef)
 LLVMValueRef)

(define-c-function
 LLVMBlockAddress
 (LLVMValueRef LLVMBasicBlockRef)
 LLVMValueRef)

(define-c-function LLVMGetBlockAddressFunction (LLVMValueRef) LLVMValueRef)

(define-c-function
 LLVMGetBlockAddressBasicBlock
 (LLVMValueRef)
 LLVMBasicBlockRef)

(define-c-function
 LLVMConstInlineAsm
 (LLVMTypeRef char-string char-string LLVMBool LLVMBool)
 LLVMValueRef)

(define-c-function LLVMGetGlobalParent (LLVMValueRef) LLVMModuleRef)

(define-c-function LLVMIsDeclaration (LLVMValueRef) LLVMBool)

(define-c-function LLVMGetLinkage (LLVMValueRef) LLVMLinkage)

(define-c-function LLVMSetLinkage (LLVMValueRef LLVMLinkage) void)

(define-c-function LLVMGetSection (LLVMValueRef) char-string)

(define-c-function LLVMSetSection (LLVMValueRef char-string) void)

(define-c-function LLVMGetVisibility (LLVMValueRef) LLVMVisibility)

(define-c-function LLVMSetVisibility (LLVMValueRef LLVMVisibility) void)

(define-c-function LLVMGetDLLStorageClass (LLVMValueRef) LLVMDLLStorageClass)

(define-c-function
 LLVMSetDLLStorageClass
 (LLVMValueRef LLVMDLLStorageClass)
 void)

(define-c-function LLVMGetUnnamedAddress (LLVMValueRef) LLVMUnnamedAddr)

(define-c-function LLVMSetUnnamedAddress (LLVMValueRef LLVMUnnamedAddr) void)

(define-c-function LLVMGlobalGetValueType (LLVMValueRef) LLVMTypeRef)

(define-c-function LLVMHasUnnamedAddr (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetUnnamedAddr (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetAlignment (LLVMValueRef) unsigned-int)

(define-c-function LLVMSetAlignment (LLVMValueRef unsigned-int) void)

(define-c-function
 LLVMGlobalSetMetadata
 (LLVMValueRef unsigned-int LLVMMetadataRef)
 void)

(define-c-function
 LLVMGlobalAddMetadata
 (LLVMValueRef unsigned-int LLVMMetadataRef)
 void)

(define-c-function LLVMGlobalEraseMetadata (LLVMValueRef unsigned-int) void)

(define-c-function LLVMGlobalClearMetadata (LLVMValueRef) void)

(define-c-function LLVMGlobalAddDebugInfo (LLVMValueRef LLVMMetadataRef) void)

(define-c-function
 LLVMGlobalCopyAllMetadata
 (LLVMValueRef (pointer size_t))
 (pointer LLVMValueMetadataEntry))

(define-c-function
 LLVMDisposeValueMetadataEntries
 ((pointer LLVMValueMetadataEntry))
 void)

(define-c-function
 LLVMValueMetadataEntriesGetKind
 ((pointer LLVMValueMetadataEntry) unsigned-int)
 unsigned-int)

(define-c-function
 LLVMValueMetadataEntriesGetMetadata
 ((pointer LLVMValueMetadataEntry) unsigned-int)
 LLVMMetadataRef)

(define-c-function
 LLVMAddGlobal
 (LLVMModuleRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMAddGlobalInAddressSpace
 (LLVMModuleRef LLVMTypeRef char-string unsigned-int)
 LLVMValueRef)

(define-c-function LLVMGetNamedGlobal (LLVMModuleRef char-string) LLVMValueRef)

(define-c-function
 LLVMGetNamedGlobalWithLength
 (LLVMModuleRef char-string size_t)
 LLVMValueRef)

(define-c-function LLVMGetFirstGlobal (LLVMModuleRef) LLVMValueRef)

(define-c-function LLVMGetLastGlobal (LLVMModuleRef) LLVMValueRef)

(define-c-function LLVMGetNextGlobal (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetPreviousGlobal (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMDeleteGlobal (LLVMValueRef) void)

(define-c-function LLVMGetInitializer (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMSetInitializer (LLVMValueRef LLVMValueRef) void)

(define-c-function LLVMIsThreadLocal (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetThreadLocal (LLVMValueRef LLVMBool) void)

(define-c-function LLVMIsGlobalConstant (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetGlobalConstant (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetThreadLocalMode (LLVMValueRef) LLVMThreadLocalMode)

(define-c-function
 LLVMSetThreadLocalMode
 (LLVMValueRef LLVMThreadLocalMode)
 void)

(define-c-function LLVMIsExternallyInitialized (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetExternallyInitialized (LLVMValueRef LLVMBool) void)

(define-c-function
 LLVMAddAlias2
 (LLVMModuleRef LLVMTypeRef unsigned-int LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMGetNamedGlobalAlias
 (LLVMModuleRef char-string size_t)
 LLVMValueRef)

(define-c-function LLVMGetFirstGlobalAlias (LLVMModuleRef) LLVMValueRef)

(define-c-function LLVMGetLastGlobalAlias (LLVMModuleRef) LLVMValueRef)

(define-c-function LLVMGetNextGlobalAlias (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetPreviousGlobalAlias (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMAliasGetAliasee (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMAliasSetAliasee (LLVMValueRef LLVMValueRef) void)

(define-c-function LLVMDeleteFunction (LLVMValueRef) void)

(define-c-function LLVMHasPersonalityFn (LLVMValueRef) LLVMBool)

(define-c-function LLVMGetPersonalityFn (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMSetPersonalityFn (LLVMValueRef LLVMValueRef) void)

(define-c-function LLVMLookupIntrinsicID (char-string size_t) unsigned-int)

(define-c-function LLVMGetIntrinsicID (LLVMValueRef) unsigned-int)

(define-c-function
 LLVMGetIntrinsicDeclaration
 (LLVMModuleRef unsigned-int (pointer LLVMTypeRef) size_t)
 LLVMValueRef)

(define-c-function
 LLVMIntrinsicGetType
 (LLVMContextRef unsigned-int (pointer LLVMTypeRef) size_t)
 LLVMTypeRef)

(define-c-function
 LLVMIntrinsicGetName
 (unsigned-int (pointer size_t))
 char-string)

(define-c-function
 LLVMIntrinsicCopyOverloadedName
 (unsigned-int (pointer LLVMTypeRef) size_t (pointer size_t))
 char-string)

(define-c-function
 LLVMIntrinsicCopyOverloadedName2
 (LLVMModuleRef unsigned-int (pointer LLVMTypeRef) size_t (pointer size_t))
 char-string)

(define-c-function LLVMIntrinsicIsOverloaded (unsigned-int) LLVMBool)

(define-c-function LLVMGetFunctionCallConv (LLVMValueRef) unsigned-int)

(define-c-function LLVMSetFunctionCallConv (LLVMValueRef unsigned-int) void)

(define-c-function LLVMGetGC (LLVMValueRef) char-string)

(define-c-function LLVMSetGC (LLVMValueRef char-string) void)

(define-c-function LLVMGetPrefixData (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMHasPrefixData (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetPrefixData (LLVMValueRef LLVMValueRef) void)

(define-c-function LLVMGetPrologueData (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMHasPrologueData (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetPrologueData (LLVMValueRef LLVMValueRef) void)

(define-c-function
 LLVMAddAttributeAtIndex
 (LLVMValueRef LLVMAttributeIndex LLVMAttributeRef)
 void)

(define-c-function
 LLVMGetAttributeCountAtIndex
 (LLVMValueRef LLVMAttributeIndex)
 unsigned-int)

(define-c-function
 LLVMGetAttributesAtIndex
 (LLVMValueRef LLVMAttributeIndex (pointer LLVMAttributeRef))
 void)

(define-c-function
 LLVMGetEnumAttributeAtIndex
 (LLVMValueRef LLVMAttributeIndex unsigned-int)
 LLVMAttributeRef)

(define-c-function
 LLVMGetStringAttributeAtIndex
 (LLVMValueRef LLVMAttributeIndex char-string unsigned-int)
 LLVMAttributeRef)

(define-c-function
 LLVMRemoveEnumAttributeAtIndex
 (LLVMValueRef LLVMAttributeIndex unsigned-int)
 void)

(define-c-function
 LLVMRemoveStringAttributeAtIndex
 (LLVMValueRef LLVMAttributeIndex char-string unsigned-int)
 void)

(define-c-function
 LLVMAddTargetDependentFunctionAttr
 (LLVMValueRef char-string char-string)
 void)

(define-c-function LLVMCountParams (LLVMValueRef) unsigned-int)

(define-c-function LLVMGetParams (LLVMValueRef (pointer LLVMValueRef)) void)

(define-c-function LLVMGetParam (LLVMValueRef unsigned-int) LLVMValueRef)

(define-c-function LLVMGetParamParent (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetFirstParam (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetLastParam (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetNextParam (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetPreviousParam (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMSetParamAlignment (LLVMValueRef unsigned-int) void)

(define-c-function
 LLVMAddGlobalIFunc
 (LLVMModuleRef char-string size_t LLVMTypeRef unsigned-int LLVMValueRef)
 LLVMValueRef)

(define-c-function
 LLVMGetNamedGlobalIFunc
 (LLVMModuleRef char-string size_t)
 LLVMValueRef)

(define-c-function LLVMGetFirstGlobalIFunc (LLVMModuleRef) LLVMValueRef)

(define-c-function LLVMGetLastGlobalIFunc (LLVMModuleRef) LLVMValueRef)

(define-c-function LLVMGetNextGlobalIFunc (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetPreviousGlobalIFunc (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetGlobalIFuncResolver (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMSetGlobalIFuncResolver (LLVMValueRef LLVMValueRef) void)

(define-c-function LLVMEraseGlobalIFunc (LLVMValueRef) void)

(define-c-function LLVMRemoveGlobalIFunc (LLVMValueRef) void)

(define-c-function
 LLVMMDStringInContext2
 (LLVMContextRef char-string size_t)
 LLVMMetadataRef)

(define-c-function
 LLVMMDNodeInContext2
 (LLVMContextRef (pointer LLVMMetadataRef) size_t)
 LLVMMetadataRef)

(define-c-function
 LLVMMetadataAsValue
 (LLVMContextRef LLVMMetadataRef)
 LLVMValueRef)

(define-c-function LLVMValueAsMetadata (LLVMValueRef) LLVMMetadataRef)

(define-c-function
 LLVMGetMDString
 (LLVMValueRef (pointer unsigned-int))
 char-string)

(define-c-function LLVMGetMDNodeNumOperands (LLVMValueRef) unsigned-int)

(define-c-function
 LLVMGetMDNodeOperands
 (LLVMValueRef (pointer LLVMValueRef))
 void)

(define-c-function
 LLVMReplaceMDNodeOperandWith
 (LLVMValueRef unsigned-int LLVMMetadataRef)
 void)

(define-c-function
 LLVMMDStringInContext
 (LLVMContextRef char-string unsigned-int)
 LLVMValueRef)

(define-c-function LLVMMDString (char-string unsigned-int) LLVMValueRef)

(define-c-function
 LLVMMDNodeInContext
 (LLVMContextRef (pointer LLVMValueRef) unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMMDNode
 ((pointer LLVMValueRef) unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMCreateOperandBundle
 (char-string size_t (pointer LLVMValueRef) unsigned-int)
 LLVMOperandBundleRef)

(define-c-function LLVMDisposeOperandBundle (LLVMOperandBundleRef) void)

(define-c-function
 LLVMGetOperandBundleTag
 (LLVMOperandBundleRef (pointer size_t))
 char-string)

(define-c-function
 LLVMGetNumOperandBundleArgs
 (LLVMOperandBundleRef)
 unsigned-int)

(define-c-function
 LLVMGetOperandBundleArgAtIndex
 (LLVMOperandBundleRef unsigned-int)
 LLVMValueRef)

(define-c-function LLVMBasicBlockAsValue (LLVMBasicBlockRef) LLVMValueRef)

(define-c-function LLVMValueIsBasicBlock (LLVMValueRef) LLVMBool)

(define-c-function LLVMValueAsBasicBlock (LLVMValueRef) LLVMBasicBlockRef)

(define-c-function LLVMGetBasicBlockName (LLVMBasicBlockRef) char-string)

(define-c-function LLVMGetBasicBlockParent (LLVMBasicBlockRef) LLVMValueRef)

(define-c-function
 LLVMGetBasicBlockTerminator
 (LLVMBasicBlockRef)
 LLVMValueRef)

(define-c-function LLVMCountBasicBlocks (LLVMValueRef) unsigned-int)

(define-c-function
 LLVMGetBasicBlocks
 (LLVMValueRef (pointer LLVMBasicBlockRef))
 void)

(define-c-function LLVMGetFirstBasicBlock (LLVMValueRef) LLVMBasicBlockRef)

(define-c-function LLVMGetLastBasicBlock (LLVMValueRef) LLVMBasicBlockRef)

(define-c-function LLVMGetNextBasicBlock (LLVMBasicBlockRef) LLVMBasicBlockRef)

(define-c-function
 LLVMGetPreviousBasicBlock
 (LLVMBasicBlockRef)
 LLVMBasicBlockRef)

(define-c-function LLVMGetEntryBasicBlock (LLVMValueRef) LLVMBasicBlockRef)

(define-c-function
 LLVMInsertExistingBasicBlockAfterInsertBlock
 (LLVMBuilderRef LLVMBasicBlockRef)
 void)

(define-c-function
 LLVMAppendExistingBasicBlock
 (LLVMValueRef LLVMBasicBlockRef)
 void)

(define-c-function
 LLVMCreateBasicBlockInContext
 (LLVMContextRef char-string)
 LLVMBasicBlockRef)

(define-c-function
 LLVMAppendBasicBlockInContext
 (LLVMContextRef LLVMValueRef char-string)
 LLVMBasicBlockRef)

(define-c-function
 LLVMAppendBasicBlock
 (LLVMValueRef char-string)
 LLVMBasicBlockRef)

(define-c-function
 LLVMInsertBasicBlockInContext
 (LLVMContextRef LLVMBasicBlockRef char-string)
 LLVMBasicBlockRef)

(define-c-function
 LLVMInsertBasicBlock
 (LLVMBasicBlockRef char-string)
 LLVMBasicBlockRef)

(define-c-function LLVMDeleteBasicBlock (LLVMBasicBlockRef) void)

(define-c-function LLVMRemoveBasicBlockFromParent (LLVMBasicBlockRef) void)

(define-c-function
 LLVMMoveBasicBlockBefore
 (LLVMBasicBlockRef LLVMBasicBlockRef)
 void)

(define-c-function
 LLVMMoveBasicBlockAfter
 (LLVMBasicBlockRef LLVMBasicBlockRef)
 void)

(define-c-function LLVMGetFirstInstruction (LLVMBasicBlockRef) LLVMValueRef)

(define-c-function LLVMGetLastInstruction (LLVMBasicBlockRef) LLVMValueRef)

(define-c-function LLVMHasMetadata (LLVMValueRef) int)

(define-c-function LLVMGetMetadata (LLVMValueRef unsigned-int) LLVMValueRef)

(define-c-function
 LLVMSetMetadata
 (LLVMValueRef unsigned-int LLVMValueRef)
 void)

(define-c-function
 LLVMInstructionGetAllMetadataOtherThanDebugLoc
 (LLVMValueRef (pointer size_t))
 (pointer LLVMValueMetadataEntry))

(define-c-function LLVMGetInstructionParent (LLVMValueRef) LLVMBasicBlockRef)

(define-c-function LLVMGetNextInstruction (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetPreviousInstruction (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMInstructionRemoveFromParent (LLVMValueRef) void)

(define-c-function LLVMInstructionEraseFromParent (LLVMValueRef) void)

(define-c-function LLVMDeleteInstruction (LLVMValueRef) void)

(define-c-function LLVMGetInstructionOpcode (LLVMValueRef) LLVMOpcode)

(define-c-function LLVMGetICmpPredicate (LLVMValueRef) LLVMIntPredicate)

(define-c-function LLVMGetICmpSameSign (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetICmpSameSign (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetFCmpPredicate (LLVMValueRef) LLVMRealPredicate)

(define-c-function LLVMInstructionClone (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMIsATerminatorInst (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetFirstDbgRecord (LLVMValueRef) LLVMDbgRecordRef)

(define-c-function LLVMGetLastDbgRecord (LLVMValueRef) LLVMDbgRecordRef)

(define-c-function LLVMGetNextDbgRecord (LLVMDbgRecordRef) LLVMDbgRecordRef)

(define-c-function
 LLVMGetPreviousDbgRecord
 (LLVMDbgRecordRef)
 LLVMDbgRecordRef)

(define-c-function LLVMDbgRecordGetDebugLoc (LLVMDbgRecordRef) LLVMMetadataRef)

(define-c-function LLVMDbgRecordGetKind (LLVMDbgRecordRef) LLVMDbgRecordKind)

(define-c-function
 LLVMDbgVariableRecordGetValue
 (LLVMDbgRecordRef unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMDbgVariableRecordGetVariable
 (LLVMDbgRecordRef)
 LLVMMetadataRef)

(define-c-function
 LLVMDbgVariableRecordGetExpression
 (LLVMDbgRecordRef)
 LLVMMetadataRef)

(define-c-function LLVMGetNumArgOperands (LLVMValueRef) unsigned-int)

(define-c-function LLVMSetInstructionCallConv (LLVMValueRef unsigned-int) void)

(define-c-function LLVMGetInstructionCallConv (LLVMValueRef) unsigned-int)

(define-c-function
 LLVMSetInstrParamAlignment
 (LLVMValueRef LLVMAttributeIndex unsigned-int)
 void)

(define-c-function
 LLVMAddCallSiteAttribute
 (LLVMValueRef LLVMAttributeIndex LLVMAttributeRef)
 void)

(define-c-function
 LLVMGetCallSiteAttributeCount
 (LLVMValueRef LLVMAttributeIndex)
 unsigned-int)

(define-c-function
 LLVMGetCallSiteAttributes
 (LLVMValueRef LLVMAttributeIndex (pointer LLVMAttributeRef))
 void)

(define-c-function
 LLVMGetCallSiteEnumAttribute
 (LLVMValueRef LLVMAttributeIndex unsigned-int)
 LLVMAttributeRef)

(define-c-function
 LLVMGetCallSiteStringAttribute
 (LLVMValueRef LLVMAttributeIndex char-string unsigned-int)
 LLVMAttributeRef)

(define-c-function
 LLVMRemoveCallSiteEnumAttribute
 (LLVMValueRef LLVMAttributeIndex unsigned-int)
 void)

(define-c-function
 LLVMRemoveCallSiteStringAttribute
 (LLVMValueRef LLVMAttributeIndex char-string unsigned-int)
 void)

(define-c-function LLVMGetCalledFunctionType (LLVMValueRef) LLVMTypeRef)

(define-c-function LLVMGetCalledValue (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMGetNumOperandBundles (LLVMValueRef) unsigned-int)

(define-c-function
 LLVMGetOperandBundleAtIndex
 (LLVMValueRef unsigned-int)
 LLVMOperandBundleRef)

(define-c-function LLVMIsTailCall (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetTailCall (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetTailCallKind (LLVMValueRef) LLVMTailCallKind)

(define-c-function LLVMSetTailCallKind (LLVMValueRef LLVMTailCallKind) void)

(define-c-function LLVMGetNormalDest (LLVMValueRef) LLVMBasicBlockRef)

(define-c-function LLVMGetUnwindDest (LLVMValueRef) LLVMBasicBlockRef)

(define-c-function LLVMSetNormalDest (LLVMValueRef LLVMBasicBlockRef) void)

(define-c-function LLVMSetUnwindDest (LLVMValueRef LLVMBasicBlockRef) void)

(define-c-function LLVMGetCallBrDefaultDest (LLVMValueRef) LLVMBasicBlockRef)

(define-c-function LLVMGetCallBrNumIndirectDests (LLVMValueRef) unsigned-int)

(define-c-function
 LLVMGetCallBrIndirectDest
 (LLVMValueRef unsigned-int)
 LLVMBasicBlockRef)

(define-c-function LLVMGetNumSuccessors (LLVMValueRef) unsigned-int)

(define-c-function
 LLVMGetSuccessor
 (LLVMValueRef unsigned-int)
 LLVMBasicBlockRef)

(define-c-function
 LLVMSetSuccessor
 (LLVMValueRef unsigned-int LLVMBasicBlockRef)
 void)

(define-c-function LLVMIsConditional (LLVMValueRef) LLVMBool)

(define-c-function LLVMGetCondition (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMSetCondition (LLVMValueRef LLVMValueRef) void)

(define-c-function LLVMGetSwitchDefaultDest (LLVMValueRef) LLVMBasicBlockRef)

(define-c-function
 LLVMGetSwitchCaseValue
 (LLVMValueRef unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMSetSwitchCaseValue
 (LLVMValueRef unsigned-int LLVMValueRef)
 void)

(define-c-function LLVMGetAllocatedType (LLVMValueRef) LLVMTypeRef)

(define-c-function LLVMIsInBounds (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetIsInBounds (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetGEPSourceElementType (LLVMValueRef) LLVMTypeRef)

(define-c-function LLVMGEPGetNoWrapFlags (LLVMValueRef) LLVMGEPNoWrapFlags)

(define-c-function
 LLVMGEPSetNoWrapFlags
 (LLVMValueRef LLVMGEPNoWrapFlags)
 void)

(define-c-function
 LLVMAddIncoming
 (LLVMValueRef (pointer LLVMValueRef) (pointer LLVMBasicBlockRef) unsigned-int)
 void)

(define-c-function LLVMCountIncoming (LLVMValueRef) unsigned-int)

(define-c-function
 LLVMGetIncomingValue
 (LLVMValueRef unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMGetIncomingBlock
 (LLVMValueRef unsigned-int)
 LLVMBasicBlockRef)

(define-c-function LLVMGetNumIndices (LLVMValueRef) unsigned-int)

(define-c-function LLVMGetIndices (LLVMValueRef) (pointer unsigned-int))

(define-c-function LLVMCreateBuilderInContext (LLVMContextRef) LLVMBuilderRef)

(define-c-function LLVMCreateBuilder () LLVMBuilderRef)

(define-c-function
 LLVMPositionBuilder
 (LLVMBuilderRef LLVMBasicBlockRef LLVMValueRef)
 void)

(define-c-function
 LLVMPositionBuilderBeforeDbgRecords
 (LLVMBuilderRef LLVMBasicBlockRef LLVMValueRef)
 void)

(define-c-function
 LLVMPositionBuilderBefore
 (LLVMBuilderRef LLVMValueRef)
 void)

(define-c-function
 LLVMPositionBuilderBeforeInstrAndDbgRecords
 (LLVMBuilderRef LLVMValueRef)
 void)

(define-c-function
 LLVMPositionBuilderAtEnd
 (LLVMBuilderRef LLVMBasicBlockRef)
 void)

(define-c-function LLVMGetInsertBlock (LLVMBuilderRef) LLVMBasicBlockRef)

(define-c-function LLVMClearInsertionPosition (LLVMBuilderRef) void)

(define-c-function LLVMInsertIntoBuilder (LLVMBuilderRef LLVMValueRef) void)

(define-c-function
 LLVMInsertIntoBuilderWithName
 (LLVMBuilderRef LLVMValueRef char-string)
 void)

(define-c-function LLVMDisposeBuilder (LLVMBuilderRef) void)

(define-c-function
 LLVMGetCurrentDebugLocation2
 (LLVMBuilderRef)
 LLVMMetadataRef)

(define-c-function
 LLVMSetCurrentDebugLocation2
 (LLVMBuilderRef LLVMMetadataRef)
 void)

(define-c-function LLVMSetInstDebugLocation (LLVMBuilderRef LLVMValueRef) void)

(define-c-function LLVMAddMetadataToInst (LLVMBuilderRef LLVMValueRef) void)

(define-c-function
 LLVMBuilderGetDefaultFPMathTag
 (LLVMBuilderRef)
 LLVMMetadataRef)

(define-c-function
 LLVMBuilderSetDefaultFPMathTag
 (LLVMBuilderRef LLVMMetadataRef)
 void)

(define-c-function LLVMGetBuilderContext (LLVMBuilderRef) LLVMContextRef)

(define-c-function
 LLVMSetCurrentDebugLocation
 (LLVMBuilderRef LLVMValueRef)
 void)

(define-c-function LLVMGetCurrentDebugLocation (LLVMBuilderRef) LLVMValueRef)

(define-c-function LLVMBuildRetVoid (LLVMBuilderRef) LLVMValueRef)

(define-c-function LLVMBuildRet (LLVMBuilderRef LLVMValueRef) LLVMValueRef)

(define-c-function
 LLVMBuildAggregateRet
 (LLVMBuilderRef (pointer LLVMValueRef) unsigned-int)
 LLVMValueRef)

(define-c-function LLVMBuildBr (LLVMBuilderRef LLVMBasicBlockRef) LLVMValueRef)

(define-c-function
 LLVMBuildCondBr
 (LLVMBuilderRef LLVMValueRef LLVMBasicBlockRef LLVMBasicBlockRef)
 LLVMValueRef)

(define-c-function
 LLVMBuildSwitch
 (LLVMBuilderRef LLVMValueRef LLVMBasicBlockRef unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMBuildIndirectBr
 (LLVMBuilderRef LLVMValueRef unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMBuildCallBr
 (LLVMBuilderRef
  LLVMTypeRef
  LLVMValueRef
  LLVMBasicBlockRef
  (pointer LLVMBasicBlockRef)
  unsigned-int
  (pointer LLVMValueRef)
  unsigned-int
  (pointer LLVMOperandBundleRef)
  unsigned-int
  char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildInvoke2
 (LLVMBuilderRef
  LLVMTypeRef
  LLVMValueRef
  (pointer LLVMValueRef)
  unsigned-int
  LLVMBasicBlockRef
  LLVMBasicBlockRef
  char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildInvokeWithOperandBundles
 (LLVMBuilderRef
  LLVMTypeRef
  LLVMValueRef
  (pointer LLVMValueRef)
  unsigned-int
  LLVMBasicBlockRef
  LLVMBasicBlockRef
  (pointer LLVMOperandBundleRef)
  unsigned-int
  char-string)
 LLVMValueRef)

(define-c-function LLVMBuildUnreachable (LLVMBuilderRef) LLVMValueRef)

(define-c-function LLVMBuildResume (LLVMBuilderRef LLVMValueRef) LLVMValueRef)

(define-c-function
 LLVMBuildLandingPad
 (LLVMBuilderRef LLVMTypeRef LLVMValueRef unsigned-int char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildCleanupRet
 (LLVMBuilderRef LLVMValueRef LLVMBasicBlockRef)
 LLVMValueRef)

(define-c-function
 LLVMBuildCatchRet
 (LLVMBuilderRef LLVMValueRef LLVMBasicBlockRef)
 LLVMValueRef)

(define-c-function
 LLVMBuildCatchPad
 (LLVMBuilderRef LLVMValueRef (pointer LLVMValueRef) unsigned-int char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildCleanupPad
 (LLVMBuilderRef LLVMValueRef (pointer LLVMValueRef) unsigned-int char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildCatchSwitch
 (LLVMBuilderRef LLVMValueRef LLVMBasicBlockRef unsigned-int char-string)
 LLVMValueRef)

(define-c-function
 LLVMAddCase
 (LLVMValueRef LLVMValueRef LLVMBasicBlockRef)
 void)

(define-c-function LLVMAddDestination (LLVMValueRef LLVMBasicBlockRef) void)

(define-c-function LLVMGetNumClauses (LLVMValueRef) unsigned-int)

(define-c-function LLVMGetClause (LLVMValueRef unsigned-int) LLVMValueRef)

(define-c-function LLVMAddClause (LLVMValueRef LLVMValueRef) void)

(define-c-function LLVMIsCleanup (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetCleanup (LLVMValueRef LLVMBool) void)

(define-c-function LLVMAddHandler (LLVMValueRef LLVMBasicBlockRef) void)

(define-c-function LLVMGetNumHandlers (LLVMValueRef) unsigned-int)

(define-c-function
 LLVMGetHandlers
 (LLVMValueRef (pointer LLVMBasicBlockRef))
 void)

(define-c-function LLVMGetArgOperand (LLVMValueRef unsigned-int) LLVMValueRef)

(define-c-function
 LLVMSetArgOperand
 (LLVMValueRef unsigned-int LLVMValueRef)
 void)

(define-c-function LLVMGetParentCatchSwitch (LLVMValueRef) LLVMValueRef)

(define-c-function LLVMSetParentCatchSwitch (LLVMValueRef LLVMValueRef) void)

(define-c-function
 LLVMBuildAdd
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildNSWAdd
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildNUWAdd
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFAdd
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildSub
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildNSWSub
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildNUWSub
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFSub
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildMul
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildNSWMul
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildNUWMul
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFMul
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildUDiv
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildExactUDiv
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildSDiv
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildExactSDiv
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFDiv
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildURem
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildSRem
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFRem
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildShl
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildLShr
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildAShr
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildAnd
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildOr
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildXor
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildBinOp
 (LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildNeg
 (LLVMBuilderRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildNSWNeg
 (LLVMBuilderRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildNUWNeg
 (LLVMBuilderRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFNeg
 (LLVMBuilderRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildNot
 (LLVMBuilderRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function LLVMGetNUW (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetNUW (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetNSW (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetNSW (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetExact (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetExact (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetNNeg (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetNNeg (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetFastMathFlags (LLVMValueRef) LLVMFastMathFlags)

(define-c-function LLVMSetFastMathFlags (LLVMValueRef LLVMFastMathFlags) void)

(define-c-function LLVMCanValueUseFastMathFlags (LLVMValueRef) LLVMBool)

(define-c-function LLVMGetIsDisjoint (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetIsDisjoint (LLVMValueRef LLVMBool) void)

(define-c-function
 LLVMBuildMalloc
 (LLVMBuilderRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildArrayMalloc
 (LLVMBuilderRef LLVMTypeRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildMemSet
 (LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMBuildMemCpy
 (LLVMBuilderRef
  LLVMValueRef
  unsigned-int
  LLVMValueRef
  unsigned-int
  LLVMValueRef)
 LLVMValueRef)

(define-c-function
 LLVMBuildMemMove
 (LLVMBuilderRef
  LLVMValueRef
  unsigned-int
  LLVMValueRef
  unsigned-int
  LLVMValueRef)
 LLVMValueRef)

(define-c-function
 LLVMBuildAlloca
 (LLVMBuilderRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildArrayAlloca
 (LLVMBuilderRef LLVMTypeRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function LLVMBuildFree (LLVMBuilderRef LLVMValueRef) LLVMValueRef)

(define-c-function
 LLVMBuildLoad2
 (LLVMBuilderRef LLVMTypeRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildStore
 (LLVMBuilderRef LLVMValueRef LLVMValueRef)
 LLVMValueRef)

(define-c-function
 LLVMBuildGEP2
 (LLVMBuilderRef
  LLVMTypeRef
  LLVMValueRef
  (pointer LLVMValueRef)
  unsigned-int
  char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildInBoundsGEP2
 (LLVMBuilderRef
  LLVMTypeRef
  LLVMValueRef
  (pointer LLVMValueRef)
  unsigned-int
  char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildGEPWithNoWrapFlags
 (LLVMBuilderRef
  LLVMTypeRef
  LLVMValueRef
  (pointer LLVMValueRef)
  unsigned-int
  char-string
  LLVMGEPNoWrapFlags)
 LLVMValueRef)

(define-c-function
 LLVMBuildStructGEP2
 (LLVMBuilderRef LLVMTypeRef LLVMValueRef unsigned-int char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildGlobalString
 (LLVMBuilderRef char-string char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildGlobalStringPtr
 (LLVMBuilderRef char-string char-string)
 LLVMValueRef)

(define-c-function LLVMGetVolatile (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetVolatile (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetWeak (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetWeak (LLVMValueRef LLVMBool) void)

(define-c-function LLVMGetOrdering (LLVMValueRef) LLVMAtomicOrdering)

(define-c-function LLVMSetOrdering (LLVMValueRef LLVMAtomicOrdering) void)

(define-c-function LLVMGetAtomicRMWBinOp (LLVMValueRef) LLVMAtomicRMWBinOp)

(define-c-function
 LLVMSetAtomicRMWBinOp
 (LLVMValueRef LLVMAtomicRMWBinOp)
 void)

(define-c-function
 LLVMBuildTrunc
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildZExt
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildSExt
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFPToUI
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFPToSI
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildUIToFP
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildSIToFP
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFPTrunc
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFPExt
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildPtrToInt
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildIntToPtr
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildBitCast
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildAddrSpaceCast
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildZExtOrBitCast
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildSExtOrBitCast
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildTruncOrBitCast
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildCast
 (LLVMBuilderRef LLVMOpcode LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildPointerCast
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildIntCast2
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef LLVMBool char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFPCast
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildIntCast
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMGetCastOpcode
 (LLVMValueRef LLVMBool LLVMTypeRef LLVMBool)
 LLVMOpcode)

(define-c-function
 LLVMBuildICmp
 (LLVMBuilderRef LLVMIntPredicate LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFCmp
 (LLVMBuilderRef LLVMRealPredicate LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildPhi
 (LLVMBuilderRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildCall2
 (LLVMBuilderRef
  LLVMTypeRef
  LLVMValueRef
  (pointer LLVMValueRef)
  unsigned-int
  char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildCallWithOperandBundles
 (LLVMBuilderRef
  LLVMTypeRef
  LLVMValueRef
  (pointer LLVMValueRef)
  unsigned-int
  (pointer LLVMOperandBundleRef)
  unsigned-int
  char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildSelect
 (LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildVAArg
 (LLVMBuilderRef LLVMValueRef LLVMTypeRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildExtractElement
 (LLVMBuilderRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildInsertElement
 (LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildShuffleVector
 (LLVMBuilderRef LLVMValueRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildExtractValue
 (LLVMBuilderRef LLVMValueRef unsigned-int char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildInsertValue
 (LLVMBuilderRef LLVMValueRef LLVMValueRef unsigned-int char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFreeze
 (LLVMBuilderRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildIsNull
 (LLVMBuilderRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildIsNotNull
 (LLVMBuilderRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildPtrDiff2
 (LLVMBuilderRef LLVMTypeRef LLVMValueRef LLVMValueRef char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFence
 (LLVMBuilderRef LLVMAtomicOrdering LLVMBool char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildFenceSyncScope
 (LLVMBuilderRef LLVMAtomicOrdering unsigned-int char-string)
 LLVMValueRef)

(define-c-function
 LLVMBuildAtomicRMW
 (LLVMBuilderRef
  LLVMAtomicRMWBinOp
  LLVMValueRef
  LLVMValueRef
  LLVMAtomicOrdering
  LLVMBool)
 LLVMValueRef)

(define-c-function
 LLVMBuildAtomicRMWSyncScope
 (LLVMBuilderRef
  LLVMAtomicRMWBinOp
  LLVMValueRef
  LLVMValueRef
  LLVMAtomicOrdering
  unsigned-int)
 LLVMValueRef)

(define-c-function
 LLVMBuildAtomicCmpXchg
 (LLVMBuilderRef
  LLVMValueRef
  LLVMValueRef
  LLVMValueRef
  LLVMAtomicOrdering
  LLVMAtomicOrdering
  LLVMBool)
 LLVMValueRef)

(define-c-function
 LLVMBuildAtomicCmpXchgSyncScope
 (LLVMBuilderRef
  LLVMValueRef
  LLVMValueRef
  LLVMValueRef
  LLVMAtomicOrdering
  LLVMAtomicOrdering
  unsigned-int)
 LLVMValueRef)

(define-c-function LLVMGetNumMaskElements (LLVMValueRef) unsigned-int)

(define-c-function LLVMGetUndefMaskElem () int)

(define-c-function LLVMGetMaskValue (LLVMValueRef unsigned-int) int)

(define-c-function LLVMIsAtomicSingleThread (LLVMValueRef) LLVMBool)

(define-c-function LLVMSetAtomicSingleThread (LLVMValueRef LLVMBool) void)

(define-c-function LLVMIsAtomic (LLVMValueRef) LLVMBool)

(define-c-function LLVMGetAtomicSyncScopeID (LLVMValueRef) unsigned-int)

(define-c-function LLVMSetAtomicSyncScopeID (LLVMValueRef unsigned-int) void)

(define-c-function
 LLVMGetCmpXchgSuccessOrdering
 (LLVMValueRef)
 LLVMAtomicOrdering)

(define-c-function
 LLVMSetCmpXchgSuccessOrdering
 (LLVMValueRef LLVMAtomicOrdering)
 void)

(define-c-function
 LLVMGetCmpXchgFailureOrdering
 (LLVMValueRef)
 LLVMAtomicOrdering)

(define-c-function
 LLVMSetCmpXchgFailureOrdering
 (LLVMValueRef LLVMAtomicOrdering)
 void)

(define-c-function
 LLVMCreateModuleProviderForExistingModule
 (LLVMModuleRef)
 LLVMModuleProviderRef)

(define-c-function LLVMDisposeModuleProvider (LLVMModuleProviderRef) void)

(define-c-function
 LLVMCreateMemoryBufferWithContentsOfFile
 (char-string (pointer LLVMMemoryBufferRef) (pointer char-string))
 LLVMBool)

(define-c-function
 LLVMCreateMemoryBufferWithSTDIN
 ((pointer LLVMMemoryBufferRef) (pointer char-string))
 LLVMBool)

(define-c-function
 LLVMCreateMemoryBufferWithMemoryRange
 (char-string size_t char-string LLVMBool)
 LLVMMemoryBufferRef)

(define-c-function
 LLVMCreateMemoryBufferWithMemoryRangeCopy
 (char-string size_t char-string)
 LLVMMemoryBufferRef)

(define-c-function LLVMGetBufferStart (LLVMMemoryBufferRef) char-string)

(define-c-function LLVMGetBufferSize (LLVMMemoryBufferRef) size_t)

(define-c-function LLVMDisposeMemoryBuffer (LLVMMemoryBufferRef) void)

(define-c-function LLVMCreatePassManager () LLVMPassManagerRef)

(define-c-function
 LLVMCreateFunctionPassManagerForModule
 (LLVMModuleRef)
 LLVMPassManagerRef)

(define-c-function
 LLVMCreateFunctionPassManager
 (LLVMModuleProviderRef)
 LLVMPassManagerRef)

(define-c-function
 LLVMRunPassManager
 (LLVMPassManagerRef LLVMModuleRef)
 LLVMBool)

(define-c-function
 LLVMInitializeFunctionPassManager
 (LLVMPassManagerRef)
 LLVMBool)

(define-c-function
 LLVMRunFunctionPassManager
 (LLVMPassManagerRef LLVMValueRef)
 LLVMBool)

(define-c-function
 LLVMFinalizeFunctionPassManager
 (LLVMPassManagerRef)
 LLVMBool)

(define-c-function LLVMDisposePassManager (LLVMPassManagerRef) void)

(define-c-function LLVMStartMultithreaded () LLVMBool)

(define-c-function LLVMStopMultithreaded () void)

(define-c-function LLVMIsMultithreaded () LLVMBool)

;;;----------------------------------------------------------------------------

;;; Target.h

(define-c-enum-type LLVMByteOrdering
 (LLVMBigEndian 0)
 (LLVMLittleEndian 1))

(define-c-type LLVMTargetDataRef (pointer (struct "LLVMOpaqueTargetData")))
(define-c-type LLVMTargetLibraryInfoRef (pointer (struct "LLVMOpaqueTargetLibraryInfotData")))

(define-c-function LLVMInitializeAArch64TargetInfo () void)

(define-c-function LLVMInitializeAMDGPUTargetInfo () void)

(define-c-function LLVMInitializeARMTargetInfo () void)

(define-c-function LLVMInitializeAVRTargetInfo () void)

(define-c-function LLVMInitializeBPFTargetInfo () void)

(define-c-function LLVMInitializeHexagonTargetInfo () void)

(define-c-function LLVMInitializeLanaiTargetInfo () void)

(define-c-function LLVMInitializeLoongArchTargetInfo () void)

(define-c-function LLVMInitializeMipsTargetInfo () void)

(define-c-function LLVMInitializeMSP430TargetInfo () void)

(define-c-function LLVMInitializeNVPTXTargetInfo () void)

(define-c-function LLVMInitializePowerPCTargetInfo () void)

(define-c-function LLVMInitializeRISCVTargetInfo () void)

(define-c-function LLVMInitializeSparcTargetInfo () void)

(define-c-function LLVMInitializeSPIRVTargetInfo () void)

(define-c-function LLVMInitializeSystemZTargetInfo () void)

(define-c-function LLVMInitializeVETargetInfo () void)

(define-c-function LLVMInitializeWebAssemblyTargetInfo () void)

(define-c-function LLVMInitializeX86TargetInfo () void)

(define-c-function LLVMInitializeXCoreTargetInfo () void)

(define-c-function LLVMInitializeAArch64Target () void)

(define-c-function LLVMInitializeAMDGPUTarget () void)

(define-c-function LLVMInitializeARMTarget () void)

(define-c-function LLVMInitializeAVRTarget () void)

(define-c-function LLVMInitializeBPFTarget () void)

(define-c-function LLVMInitializeHexagonTarget () void)

(define-c-function LLVMInitializeLanaiTarget () void)

(define-c-function LLVMInitializeLoongArchTarget () void)

(define-c-function LLVMInitializeMipsTarget () void)

(define-c-function LLVMInitializeMSP430Target () void)

(define-c-function LLVMInitializeNVPTXTarget () void)

(define-c-function LLVMInitializePowerPCTarget () void)

(define-c-function LLVMInitializeRISCVTarget () void)

(define-c-function LLVMInitializeSparcTarget () void)

(define-c-function LLVMInitializeSPIRVTarget () void)

(define-c-function LLVMInitializeSystemZTarget () void)

(define-c-function LLVMInitializeVETarget () void)

(define-c-function LLVMInitializeWebAssemblyTarget () void)

(define-c-function LLVMInitializeX86Target () void)

(define-c-function LLVMInitializeXCoreTarget () void)

(define-c-function LLVMInitializeAArch64TargetMC () void)

(define-c-function LLVMInitializeAMDGPUTargetMC () void)

(define-c-function LLVMInitializeARMTargetMC () void)

(define-c-function LLVMInitializeAVRTargetMC () void)

(define-c-function LLVMInitializeBPFTargetMC () void)

(define-c-function LLVMInitializeHexagonTargetMC () void)

(define-c-function LLVMInitializeLanaiTargetMC () void)

(define-c-function LLVMInitializeLoongArchTargetMC () void)

(define-c-function LLVMInitializeMipsTargetMC () void)

(define-c-function LLVMInitializeMSP430TargetMC () void)

(define-c-function LLVMInitializeNVPTXTargetMC () void)

(define-c-function LLVMInitializePowerPCTargetMC () void)

(define-c-function LLVMInitializeRISCVTargetMC () void)

(define-c-function LLVMInitializeSparcTargetMC () void)

(define-c-function LLVMInitializeSPIRVTargetMC () void)

(define-c-function LLVMInitializeSystemZTargetMC () void)

(define-c-function LLVMInitializeVETargetMC () void)

(define-c-function LLVMInitializeWebAssemblyTargetMC () void)

(define-c-function LLVMInitializeX86TargetMC () void)

(define-c-function LLVMInitializeXCoreTargetMC () void)

(define-c-function LLVMInitializeAArch64AsmPrinter () void)

(define-c-function LLVMInitializeAMDGPUAsmPrinter () void)

(define-c-function LLVMInitializeARMAsmPrinter () void)

(define-c-function LLVMInitializeAVRAsmPrinter () void)

(define-c-function LLVMInitializeBPFAsmPrinter () void)

(define-c-function LLVMInitializeHexagonAsmPrinter () void)

(define-c-function LLVMInitializeLanaiAsmPrinter () void)

(define-c-function LLVMInitializeLoongArchAsmPrinter () void)

(define-c-function LLVMInitializeMipsAsmPrinter () void)

(define-c-function LLVMInitializeMSP430AsmPrinter () void)

(define-c-function LLVMInitializeNVPTXAsmPrinter () void)

(define-c-function LLVMInitializePowerPCAsmPrinter () void)

(define-c-function LLVMInitializeRISCVAsmPrinter () void)

(define-c-function LLVMInitializeSparcAsmPrinter () void)

(define-c-function LLVMInitializeSPIRVAsmPrinter () void)

(define-c-function LLVMInitializeSystemZAsmPrinter () void)

(define-c-function LLVMInitializeVEAsmPrinter () void)

(define-c-function LLVMInitializeWebAssemblyAsmPrinter () void)

(define-c-function LLVMInitializeX86AsmPrinter () void)

(define-c-function LLVMInitializeXCoreAsmPrinter () void)

(define-c-function LLVMInitializeAArch64AsmParser () void)

(define-c-function LLVMInitializeAMDGPUAsmParser () void)

(define-c-function LLVMInitializeARMAsmParser () void)

(define-c-function LLVMInitializeAVRAsmParser () void)

(define-c-function LLVMInitializeBPFAsmParser () void)

(define-c-function LLVMInitializeHexagonAsmParser () void)

(define-c-function LLVMInitializeLanaiAsmParser () void)

(define-c-function LLVMInitializeLoongArchAsmParser () void)

(define-c-function LLVMInitializeMipsAsmParser () void)

(define-c-function LLVMInitializeMSP430AsmParser () void)

(define-c-function LLVMInitializePowerPCAsmParser () void)

(define-c-function LLVMInitializeRISCVAsmParser () void)

(define-c-function LLVMInitializeSparcAsmParser () void)

(define-c-function LLVMInitializeSystemZAsmParser () void)

(define-c-function LLVMInitializeVEAsmParser () void)

(define-c-function LLVMInitializeWebAssemblyAsmParser () void)

(define-c-function LLVMInitializeX86AsmParser () void)

(define-c-function LLVMInitializeAArch64Disassembler () void)

(define-c-function LLVMInitializeAMDGPUDisassembler () void)

(define-c-function LLVMInitializeARMDisassembler () void)

(define-c-function LLVMInitializeAVRDisassembler () void)

(define-c-function LLVMInitializeBPFDisassembler () void)

(define-c-function LLVMInitializeHexagonDisassembler () void)

(define-c-function LLVMInitializeLanaiDisassembler () void)

(define-c-function LLVMInitializeLoongArchDisassembler () void)

(define-c-function LLVMInitializeMipsDisassembler () void)

(define-c-function LLVMInitializeMSP430Disassembler () void)

(define-c-function LLVMInitializePowerPCDisassembler () void)

(define-c-function LLVMInitializeRISCVDisassembler () void)

(define-c-function LLVMInitializeSparcDisassembler () void)

(define-c-function LLVMInitializeSystemZDisassembler () void)

(define-c-function LLVMInitializeVEDisassembler () void)

(define-c-function LLVMInitializeWebAssemblyDisassembler () void)

(define-c-function LLVMInitializeX86Disassembler () void)

(define-c-function LLVMInitializeXCoreDisassembler () void)

(define-c-function LLVMInitializeAllTargetInfos () void)

(define-c-function LLVMInitializeAllTargets () void)

(define-c-function LLVMInitializeAllTargetMCs () void)

(define-c-function LLVMInitializeAllAsmPrinters () void)

(define-c-function LLVMInitializeAllAsmParsers () void)

(define-c-function LLVMInitializeAllDisassemblers () void)

(define-c-function LLVMInitializeNativeTarget () LLVMBool)

(define-c-function LLVMInitializeNativeAsmParser () LLVMBool)

(define-c-function LLVMInitializeNativeAsmPrinter () LLVMBool)

(define-c-function LLVMInitializeNativeDisassembler () LLVMBool)

(define-c-function LLVMGetModuleDataLayout (LLVMModuleRef) LLVMTargetDataRef)

(define-c-function
 LLVMSetModuleDataLayout
 (LLVMModuleRef LLVMTargetDataRef)
 void)

(define-c-function LLVMCreateTargetData (char-string) LLVMTargetDataRef)

(define-c-function LLVMDisposeTargetData (LLVMTargetDataRef) void)

(define-c-function
 LLVMAddTargetLibraryInfo
 (LLVMTargetLibraryInfoRef LLVMPassManagerRef)
 void)

(define-c-function
 LLVMCopyStringRepOfTargetData
 (LLVMTargetDataRef)
 char-string)

(define-c-function LLVMByteOrder (LLVMTargetDataRef) LLVMByteOrdering)

(define-c-function LLVMPointerSize (LLVMTargetDataRef) unsigned-int)

(define-c-function
 LLVMPointerSizeForAS
 (LLVMTargetDataRef unsigned-int)
 unsigned-int)

(define-c-function LLVMIntPtrType (LLVMTargetDataRef) LLVMTypeRef)

(define-c-function
 LLVMIntPtrTypeForAS
 (LLVMTargetDataRef unsigned-int)
 LLVMTypeRef)

(define-c-function
 LLVMIntPtrTypeInContext
 (LLVMContextRef LLVMTargetDataRef)
 LLVMTypeRef)

(define-c-function
 LLVMIntPtrTypeForASInContext
 (LLVMContextRef LLVMTargetDataRef unsigned-int)
 LLVMTypeRef)

(define-c-function
 LLVMSizeOfTypeInBits
 (LLVMTargetDataRef LLVMTypeRef)
 unsigned-long-long)

(define-c-function
 LLVMStoreSizeOfType
 (LLVMTargetDataRef LLVMTypeRef)
 unsigned-long-long)

(define-c-function
 LLVMABISizeOfType
 (LLVMTargetDataRef LLVMTypeRef)
 unsigned-long-long)

(define-c-function
 LLVMABIAlignmentOfType
 (LLVMTargetDataRef LLVMTypeRef)
 unsigned-int)

(define-c-function
 LLVMCallFrameAlignmentOfType
 (LLVMTargetDataRef LLVMTypeRef)
 unsigned-int)

(define-c-function
 LLVMPreferredAlignmentOfType
 (LLVMTargetDataRef LLVMTypeRef)
 unsigned-int)

(define-c-function
 LLVMPreferredAlignmentOfGlobal
 (LLVMTargetDataRef LLVMValueRef)
 unsigned-int)

(define-c-function
 LLVMElementAtOffset
 (LLVMTargetDataRef LLVMTypeRef unsigned-long-long)
 unsigned-int)

(define-c-function
 LLVMOffsetOfElement
 (LLVMTargetDataRef LLVMTypeRef unsigned-int)
 unsigned-long-long)

;;;----------------------------------------------------------------------------

;;; TargetMachine.h

(define-c-type LLVMTargetMachineOptionsRef (pointer (struct "LLVMOpaqueTargetMachineOptions")))
(define-c-type LLVMTargetMachineRef (pointer (struct "LLVMOpaqueTargetMachine")))
(define-c-type LLVMTargetRef (pointer (struct "LLVMTarget")))

(define-c-enum-type LLVMCodeGenOptLevel
  (LLVMCodeGenLevelNone 0)
  (LLVMCodeGenLevelLess 1)
  (LLVMCodeGenLevelDefault 2)
  (LLVMCodeGenLevelAggressive 3))

(define-c-enum-type LLVMRelocMode
  (LLVMRelocDefault 0)
  (LLVMRelocStatic 1)
  (LLVMRelocPIC 2)
  (LLVMRelocDynamicNoPic 3)
  (LLVMRelocROPI 4)
  (LLVMRelocRWPI 5)
  (LLVMRelocROPI_RWPI 6))

(define-c-enum-type LLVMCodeModel
  (LLVMCodeModelDefault 0)
  (LLVMCodeModelJITDefault 1)
  (LLVMCodeModelTiny 2)
  (LLVMCodeModelSmall 3)
  (LLVMCodeModelKernel 4)
  (LLVMCodeModelMedium 5)
  (LLVMCodeModelLarge 6))

(define-c-enum-type LLVMCodeGenFileType
  (LLVMAssemblyFile 0)
  (LLVMObjectFile 1))

(define-c-enum-type LLVMGlobalISelAbortMode
  (LLVMGlobalISelAbortEnable 0)
  (LLVMGlobalISelAbortDisable 1)
  (LLVMGlobalISelAbortDisableWithDiag 2))

(define-c-function LLVMGetFirstTarget () LLVMTargetRef)

(define-c-function LLVMGetNextTarget (LLVMTargetRef) LLVMTargetRef)

(define-c-function LLVMGetTargetFromName (char-string) LLVMTargetRef)

(define-c-function
 LLVMGetTargetFromTriple ;; see wrapper below
 (char-string (pointer LLVMTargetRef) (pointer char-string))
 LLVMBool)

(define-c-function LLVMGetTargetName (LLVMTargetRef) char-string)

(define-c-function LLVMGetTargetDescription (LLVMTargetRef) char-string)

(define-c-function LLVMTargetHasJIT (LLVMTargetRef) LLVMBool)

(define-c-function LLVMTargetHasTargetMachine (LLVMTargetRef) LLVMBool)

(define-c-function LLVMTargetHasAsmBackend (LLVMTargetRef) LLVMBool)

(define-c-function
 LLVMCreateTargetMachineOptions
 ()
 LLVMTargetMachineOptionsRef)

(define-c-function
 LLVMDisposeTargetMachineOptions
 (LLVMTargetMachineOptionsRef)
 void)

(define-c-function
 LLVMTargetMachineOptionsSetCPU
 (LLVMTargetMachineOptionsRef char-string)
 void)

(define-c-function
 LLVMTargetMachineOptionsSetFeatures
 (LLVMTargetMachineOptionsRef char-string)
 void)

(define-c-function
 LLVMTargetMachineOptionsSetABI
 (LLVMTargetMachineOptionsRef char-string)
 void)

(define-c-function
 LLVMTargetMachineOptionsSetCodeGenOptLevel
 (LLVMTargetMachineOptionsRef LLVMCodeGenOptLevel)
 void)

(define-c-function
 LLVMTargetMachineOptionsSetRelocMode
 (LLVMTargetMachineOptionsRef LLVMRelocMode)
 void)

(define-c-function
 LLVMTargetMachineOptionsSetCodeModel
 (LLVMTargetMachineOptionsRef LLVMCodeModel)
 void)

(define-c-function
 LLVMCreateTargetMachineWithOptions
 (LLVMTargetRef char-string LLVMTargetMachineOptionsRef)
 LLVMTargetMachineRef)

(define-c-function
 LLVMCreateTargetMachine
 (LLVMTargetRef
  char-string
  char-string
  char-string
  LLVMCodeGenOptLevel
  LLVMRelocMode
  LLVMCodeModel)
 LLVMTargetMachineRef)

(define-c-function LLVMDisposeTargetMachine (LLVMTargetMachineRef) void)

(define-c-function
 LLVMGetTargetMachineTarget
 (LLVMTargetMachineRef)
 LLVMTargetRef)

(define-c-function
 LLVMGetTargetMachineTriple
 (LLVMTargetMachineRef)
 char-string)

(define-c-function LLVMGetTargetMachineCPU (LLVMTargetMachineRef) char-string)

(define-c-function
 LLVMGetTargetMachineFeatureString
 (LLVMTargetMachineRef)
 char-string)

(define-c-function
 LLVMCreateTargetDataLayout
 (LLVMTargetMachineRef)
 LLVMTargetDataRef)

(define-c-function
 LLVMSetTargetMachineAsmVerbosity
 (LLVMTargetMachineRef LLVMBool)
 void)

(define-c-function
 LLVMSetTargetMachineFastISel
 (LLVMTargetMachineRef LLVMBool)
 void)

(define-c-function
 LLVMSetTargetMachineGlobalISel
 (LLVMTargetMachineRef LLVMBool)
 void)

(define-c-function
 LLVMSetTargetMachineGlobalISelAbort
 (LLVMTargetMachineRef LLVMGlobalISelAbortMode)
 void)

(define-c-function
 LLVMSetTargetMachineMachineOutliner
 (LLVMTargetMachineRef LLVMBool)
 void)

(define-c-function
 LLVMTargetMachineEmitToFile ;; see wrapper below
 (LLVMTargetMachineRef
  LLVMModuleRef
  char-string
  LLVMCodeGenFileType
  (pointer char-string))
 LLVMBool)

(define-c-function
 LLVMTargetMachineEmitToMemoryBuffer
 (LLVMTargetMachineRef
  LLVMModuleRef
  LLVMCodeGenFileType
  (pointer char-string)
  (pointer LLVMMemoryBufferRef))
 LLVMBool)

(define-c-function LLVMGetDefaultTargetTriple () char-string)

(define-c-function LLVMNormalizeTargetTriple (char-string) char-string)

(define-c-function LLVMGetHostCPUName () char-string)

(define-c-function LLVMGetHostCPUFeatures () char-string)

(define-c-function
 LLVMAddAnalysisPasses
 (LLVMTargetMachineRef LLVMPassManagerRef)
 void)

;;;----------------------------------------------------------------------------

;;; Transforms/PassBuilder.h

(define-c-type LLVMPassBuilderOptionsRef (pointer (struct "LLVMOpaquePassBuilderOptions")))
(define-c-type LLVMErrorRef (pointer (struct "LLVMOpaqueError")))

(define-c-function
 LLVMAddAnalysisPasses
 (LLVMTargetMachineRef LLVMPassManagerRef)
 void)

(define-c-function
 LLVMRunPasses
 (LLVMModuleRef char-string LLVMTargetMachineRef LLVMPassBuilderOptionsRef)
 LLVMErrorRef)

(define-c-function
 LLVMRunPassesOnFunction
 (LLVMValueRef char-string LLVMTargetMachineRef LLVMPassBuilderOptionsRef)
 LLVMErrorRef)

(define-c-function LLVMCreatePassBuilderOptions () LLVMPassBuilderOptionsRef)

(define-c-function
 LLVMPassBuilderOptionsSetVerifyEach
 (LLVMPassBuilderOptionsRef LLVMBool)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetDebugLogging
 (LLVMPassBuilderOptionsRef LLVMBool)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetAAPipeline
 (LLVMPassBuilderOptionsRef char-string)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetLoopInterleaving
 (LLVMPassBuilderOptionsRef LLVMBool)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetLoopVectorization
 (LLVMPassBuilderOptionsRef LLVMBool)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetSLPVectorization
 (LLVMPassBuilderOptionsRef LLVMBool)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetLoopUnrolling
 (LLVMPassBuilderOptionsRef LLVMBool)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetForgetAllSCEVInLoopUnroll
 (LLVMPassBuilderOptionsRef LLVMBool)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetLicmMssaOptCap
 (LLVMPassBuilderOptionsRef unsigned-int)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetLicmMssaNoAccForPromotionCap
 (LLVMPassBuilderOptionsRef unsigned-int)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetCallGraphProfile
 (LLVMPassBuilderOptionsRef LLVMBool)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetMergeFunctions
 (LLVMPassBuilderOptionsRef LLVMBool)
 void)

(define-c-function
 LLVMPassBuilderOptionsSetInlinerThreshold
 (LLVMPassBuilderOptionsRef int)
 void)

(define-c-function
 LLVMDisposePassBuilderOptions
 (LLVMPassBuilderOptionsRef)
 void)

;;;----------------------------------------------------------------------------

;;; More Scheme-friendly procedures to access LLVM version.

(define (LLVMGetVersion-as-vector)
  (let ((major (c-array-of-unsigned-int-alloc 1))
        (minor (c-array-of-unsigned-int-alloc 1))
        (patch (c-array-of-unsigned-int-alloc 1)))
    (LLVMGetVersion major minor patch)
    (let ((result
           (vector (c-array-of-unsigned-int-get major 0)
                   (c-array-of-unsigned-int-get minor 0)
                   (c-array-of-unsigned-int-get patch 0))))
      (c-array-of-unsigned-int-free major)
      (c-array-of-unsigned-int-free minor)
      (c-array-of-unsigned-int-free patch)
      result)))

(define (LLVMGetVersion-as-string)
  (let ((version (LLVMGetVersion-as-vector)))
    (string-append (number->string (vector-ref version 0))
                   "."
                   (number->string (vector-ref version 1))
                   "."
                   (number->string (vector-ref version 2)))))

;;; char-string arrays

(define-c-array-of char-string "char*" "char_string")

(define (char-string-array . strs)
  (list->char-string-array strs))

(define (list->char-string-array strs)
  (let* ((size (length strs))
         (array (c-array-of-char-string-alloc size)))
    (let loop ((lst strs) (i 0))
      (if (pair? lst)
          (begin
            (c-array-of-char-string-set array i (car lst))
            (loop (cdr lst) (+ i 1)))
          array))))

;;; LLVMTypeRef arrays

(define-c-array-of LLVMTypeRef)

(define (LLVMTypeRef-array . types)
  (list->LLVMTypeRef-array types))

(define (list->LLVMTypeRef-array types)
  (let* ((size (length types))
         (array (c-array-of-LLVMTypeRef-alloc size)))
    (let loop ((lst types) (i 0))
      (if (pair? lst)
          (begin
            (c-array-of-LLVMTypeRef-set array i (car lst))
            (loop (cdr lst) (+ i 1)))
          array))))

;;; LLVMValueRef arrays

(define-c-array-of LLVMValueRef)

(define (LLVMValueRef-array . values)
  (list->LLVMValueRef-array values))

(define (list->LLVMValueRef-array values)
  (let* ((size (length values))
         (array (c-array-of-LLVMValueRef-alloc size)))
    (let loop ((lst values) (i 0))
      (if (pair? lst)
          (begin
            (c-array-of-LLVMValueRef-set array i (car lst))
            (loop (cdr lst) (+ i 1)))
          array))))

;;; LLVMBasicBlockRef arrays

(define-c-array-of LLVMBasicBlockRef)

(define (LLVMBasicBlockRef-array . basic-blocks)
  (list->LLVMBasicBlockRef-array basic-blocks))

(define (list->LLVMBasicBlockRef-array basic-blocks)
  (let* ((size (length basic-blocks))
         (array (c-array-of-LLVMBasicBlockRef-alloc size)))
    (let loop ((lst basic-blocks) (i 0))
      (if (pair? lst)
          (begin
            (c-array-of-LLVMBasicBlockRef-set array i (car lst))
            (loop (cdr lst) (+ i 1)))
          array))))

;;; LLVMTargetRef arrays

(define-c-array-of LLVMTargetRef)

(define (LLVMTargetRef-array . targets)
  (list->LLVMTargetRef-array targets))

(define (list->LLVMTargetRef-array targets)
  (let* ((size (length targets))
         (array (c-array-of-LLVMTargetRef-alloc size)))
    (let loop ((lst targets) (i 0))
      (if (pair? lst)
          (begin
            (c-array-of-LLVMTargetRef-set array i (car lst))
            (loop (cdr lst) (+ i 1)))
          array))))

(define (_LLVMGetTargetFromTriple triple)
  (let* ((t (c-array-of-LLVMTargetRef-alloc 1))
         (e (c-array-of-char-string-alloc 1))
         (error? (LLVMGetTargetFromTriple triple t e)))
    (if error?
        (begin
          (c-array-of-LLVMTargetRef-free t)
          (c-array-of-char-string-free e)
          #f)
        (let ((result (c-array-of-LLVMTargetRef-get t 0)))
          (c-array-of-LLVMTargetRef-free t)
          (c-array-of-char-string-free e)
          result))))

(define (_LLVMTargetMachineEmitToFile tm mod path kind)
  (let* ((e (c-array-of-char-string-alloc 1))
         (error? (LLVMTargetMachineEmitToFile tm mod path kind e)))
    (if error?
        (let ((msg (c-array-of-char-string-get e 0)))
          (c-array-of-char-string-free e)
          msg)
        (begin
          (c-array-of-char-string-free e)
          #f))))

;;;============================================================================
