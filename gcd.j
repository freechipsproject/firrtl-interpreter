.class public gcd/GCDImpl
.super java/lang/Object

.field public values [I

.method public values()[I
  .limit stack 2
  .limit locals 1

  aload_0
  getfield gcd/GCDImpl/values [I
  areturn
.end method


.method public <init>([I)V
  .limit stack 3
  .limit locals 2

  aload_0
  aload_1
  putfield gcd/GCDImpl/values [I
  aload_0
  invokespecial java/lang/Object/<init>()V

  return
.end method

.method public add(II)I
  .limit stack 3
  .limit locals 3
  iload_1
  iload_2
  invokestatic firrtl_interpreter/executable/OperationImplementations/addInt(II)I
  ireturn
.end method

.method public step()V
  .limit stack  100
  .limit locals 12

      ; AssignIntValuesNode(6, GetIntValuesNode(5))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            6
      
      ; GetIntValuesNode(5)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            5
      iaload
      
      ; finish AssignIntValuesNode(6, GetIntValuesNode(5))
      iastore
      
      ; AssignIntValuesNode(8, GetIntValuesNode(7))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            8
      
      ; GetIntValuesNode(7)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            7
      iaload
      
      ; finish AssignIntValuesNode(8, GetIntValuesNode(7))
      iastore
      
      ; AssignIntValuesNode(9, GtIntValuesNode(GetIntValuesNode(6),GetIntValuesNode(8)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            9
      
      ; GtIntValuesNode(GetIntValuesNode(6), GetIntValuesNode(8))
      
      ; GetIntValuesNode(6)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            6
      iaload
      
      
      ; GetIntValuesNode(8)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            8
      iaload
      
      ; finish GtIntValuesNode(GetIntValuesNode(6), GetIntValuesNode(8))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/gtInt(II)I
      
      ; finish AssignIntValuesNode(9, GtIntValuesNode(GetIntValuesNode(6),GetIntValuesNode(8)))
      iastore
      
      ; AssignIntValuesNode(10, SubIntValuesNode(GetIntValuesNode(6),GetIntValuesNode(8)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            10
      
      ; SubIntValuesNode(GetIntValuesNode(6), GetIntValuesNode(8))
      
      ; GetIntValuesNode(6)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            6
      iaload
      
      
      ; GetIntValuesNode(8)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            8
      iaload
      
      ; finish SubIntValuesNode(GetIntValuesNode(6), GetIntValuesNode(8))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/subInt(II)I
      
      ; finish AssignIntValuesNode(10, SubIntValuesNode(GetIntValuesNode(6),GetIntValuesNode(8)))
      iastore
      
      ; AssignIntValuesNode(11, TailIntValuesNode(GetIntValuesNode(10),GetIntValuesConstantNode(1)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            11
      
      ; TailIntValuesNode(GetIntValuesNode(10), GetIntValuesConstantNode(1))
      
      ; GetIntValuesNode(10)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            10
      iaload
      
      
      ; GetIntValuesConstantNode(1)
      iconst_1
      
      ; finish TailIntValuesNode(GetIntValuesNode(10), GetIntValuesConstantNode(1))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/tailInt(II)I
      
      ; finish AssignIntValuesNode(11, TailIntValuesNode(GetIntValuesNode(10),GetIntValuesConstantNode(1)))
      iastore
      
      ; AssignIntValuesNode(13, EqIntValuesNode(GetIntValuesNode(9),GetIntValuesConstantNode(0)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            13
      
      ; EqIntValuesNode(GetIntValuesNode(9), GetIntValuesConstantNode(0))
      
      ; GetIntValuesNode(9)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            9
      iaload
      
      
      ; GetIntValuesConstantNode(0)
      iconst_0
      
      ; finish EqIntValuesNode(GetIntValuesNode(9), GetIntValuesConstantNode(0))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/eqInt(II)I
      
      ; finish AssignIntValuesNode(13, EqIntValuesNode(GetIntValuesNode(9),GetIntValuesConstantNode(0)))
      iastore
      
      ; AssignIntValuesNode(14, SubIntValuesNode(GetIntValuesNode(8),GetIntValuesNode(6)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            14
      
      ; SubIntValuesNode(GetIntValuesNode(8), GetIntValuesNode(6))
      
      ; GetIntValuesNode(8)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            8
      iaload
      
      
      ; GetIntValuesNode(6)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            6
      iaload
      
      ; finish SubIntValuesNode(GetIntValuesNode(8), GetIntValuesNode(6))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/subInt(II)I
      
      ; finish AssignIntValuesNode(14, SubIntValuesNode(GetIntValuesNode(8),GetIntValuesNode(6)))
      iastore
      
      ; AssignIntValuesNode(15, TailIntValuesNode(GetIntValuesNode(14),GetIntValuesConstantNode(1)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            15
      
      ; TailIntValuesNode(GetIntValuesNode(14), GetIntValuesConstantNode(1))
      
      ; GetIntValuesNode(14)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            14
      iaload
      
      
      ; GetIntValuesConstantNode(1)
      iconst_1
      
      ; finish TailIntValuesNode(GetIntValuesNode(14), GetIntValuesConstantNode(1))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/tailInt(II)I
      
      ; finish AssignIntValuesNode(15, TailIntValuesNode(GetIntValuesNode(14),GetIntValuesConstantNode(1)))
      iastore
      
      ; AssignIntValuesNode(17, EqIntValuesNode(GetIntValuesNode(8),GetIntValuesConstantNode(0)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            17
      
      ; EqIntValuesNode(GetIntValuesNode(8), GetIntValuesConstantNode(0))
      
      ; GetIntValuesNode(8)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            8
      iaload
      
      
      ; GetIntValuesConstantNode(0)
      iconst_0
      
      ; finish EqIntValuesNode(GetIntValuesNode(8), GetIntValuesConstantNode(0))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/eqInt(II)I
      
      ; finish AssignIntValuesNode(17, EqIntValuesNode(GetIntValuesNode(8),GetIntValuesConstantNode(0)))
      iastore
      
      ; AssignIntValuesNode(18, MuxIntValuesNode(GetIntValuesNode(9),GetIntValuesNode(11),GetIntValuesNode(6)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            18
      
      ; MuxIntValuesNode(GetIntValuesNode(9), GetIntValuesNode(11), GetIntValuesNode(6))
      
      ; GetIntValuesNode(9)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            9
      iaload
      
      
      ; GetIntValuesNode(11)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            11
      iaload
      
      
      ; GetIntValuesNode(6)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            6
      iaload
      
      ; finish MuxIntValuesNode(GetIntValuesNode(9), GetIntValuesNode(11), GetIntValuesNode(6))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/muxInt(III)I
      
      ; finish AssignIntValuesNode(18, MuxIntValuesNode(GetIntValuesNode(9),GetIntValuesNode(11),GetIntValuesNode(6)))
      iastore
      
      ; AssignIntValuesNode(19, MuxIntValuesNode(GetIntValuesNode(13),GetIntValuesNode(15),GetIntValuesNode(8)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            19
      
      ; MuxIntValuesNode(GetIntValuesNode(13), GetIntValuesNode(15), GetIntValuesNode(8))
      
      ; GetIntValuesNode(13)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            13
      iaload
      
      
      ; GetIntValuesNode(15)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            15
      iaload
      
      
      ; GetIntValuesNode(8)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            8
      iaload
      
      ; finish MuxIntValuesNode(GetIntValuesNode(13), GetIntValuesNode(15), GetIntValuesNode(8))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/muxInt(III)I
      
      ; finish AssignIntValuesNode(19, MuxIntValuesNode(GetIntValuesNode(13),GetIntValuesNode(15),GetIntValuesNode(8)))
      iastore
      
      ; AssignIntValuesNode(3, GetIntValuesNode(6))
      aload_0
      getfield       gcd/GCDImpl/values [I
      iconst_3
      
      ; GetIntValuesNode(6)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            6
      iaload
      
      ; finish AssignIntValuesNode(3, GetIntValuesNode(6))
      iastore
      
      ; AssignIntValuesNode(4, GetIntValuesNode(17))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            4
      
      ; GetIntValuesNode(17)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            17
      iaload
      
      ; finish AssignIntValuesNode(4, GetIntValuesNode(17))
      iastore
      
      ; AssignIntValuesNode(5, GetIntValuesNode(17))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            5
      
      ; GetIntValuesNode(17)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            17
      iaload
      
      ; finish AssignIntValuesNode(5, GetIntValuesNode(17))
      iastore
      
      ; AssignIntValuesNode(5, MuxIntValuesNode(GetIntValuesNode(2),GetIntValuesNode(0),GetIntValuesNode(18)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            5
      
      ; MuxIntValuesNode(GetIntValuesNode(2), GetIntValuesNode(0), GetIntValuesNode(18))
      
      ; GetIntValuesNode(2)
      aload_0
      getfield       gcd/GCDImpl/values [I
      iconst_2
      iaload
      
      
      ; GetIntValuesNode(0)
      aload_0
      getfield       gcd/GCDImpl/values [I
      iconst_0
      iaload
      
      
      ; GetIntValuesNode(18)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            18
      iaload
      
      ; finish MuxIntValuesNode(GetIntValuesNode(2), GetIntValuesNode(0), GetIntValuesNode(18))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/muxInt(III)I
      
      ; finish AssignIntValuesNode(5, MuxIntValuesNode(GetIntValuesNode(2),GetIntValuesNode(0),GetIntValuesNode(18)))
      iastore
      
      ; AssignIntValuesNode(7, MuxIntValuesNode(GetIntValuesNode(2),GetIntValuesNode(1),GetIntValuesNode(19)))
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            7
      
      ; MuxIntValuesNode(GetIntValuesNode(2), GetIntValuesNode(1), GetIntValuesNode(19))
      
      ; GetIntValuesNode(2)
      aload_0
      getfield       gcd/GCDImpl/values [I
      iconst_2
      iaload
      
      
      ; GetIntValuesNode(1)
      aload_0
      getfield       gcd/GCDImpl/values [I
      iconst_1
      iaload
      
      
      ; GetIntValuesNode(19)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ldc            19
      iaload
      
      ; finish MuxIntValuesNode(GetIntValuesNode(2), GetIntValuesNode(1), GetIntValuesNode(19))
      invokestatic   firrtl_interpreter/executable/OperationImplementations/muxInt(III)I
      
      ; finish AssignIntValuesNode(7, MuxIntValuesNode(GetIntValuesNode(2),GetIntValuesNode(1),GetIntValuesNode(19)))
      iastore
      

      return



;;   ; get 0
;;   aload_0
;;   getfield  gcd/GCDImpl/values [I
;;   iconst_1
;;   iaload
;;   istore_1
;; 
;;   aload_0
;;   getfield  gcd/GCDImpl/values [I
;;   iconst_0
;;   iaload
;;   istore_2
;; 
;;   iload_1
;;   iload_2
;;   invokestatic firrtl_interpreter/executable/OperationImplementations/addInt(II)I
;;   istore_2
;; 
;;   aload_0
;;   getfield  gcd/GCDImpl/values [I
;;   iconst_2
;;   iload_2
;;   iastore
;; 
;;   return
.end method

; .method public static main([Ljava/lang/String;)V
;   .limit stack 3
;   .limit locals 1
; 
; 
;   getstatic      java/lang/System/out Ljava/io/PrintStream;
;   aload_0
;   invokevirtual  java/io/PrintStream/println([I)V
; 
;   return
; .end method

