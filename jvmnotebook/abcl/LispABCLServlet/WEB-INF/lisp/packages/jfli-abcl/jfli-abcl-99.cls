����  - M execute A([Lorg/armedbear/lisp/LispObject;)Lorg/armedbear/lisp/LispObject; G11959_APPLY Lorg/armedbear/lisp/Symbol; APPLY  COMMON-LISP  JARRAY-REF-RAW 	 JAVA  G11960_JARRAY_REF_RAW Lorg/armedbear/lisp/LispObject; org/armedbear/lisp/Lisp  interrupted Z  	   handleInterrupt ()V  
   org/armedbear/lisp/jfli_abcl_99  fastProcessArgs B([Lorg/armedbear/lisp/LispObject;)[Lorg/armedbear/lisp/LispObject;  
   org/armedbear/lisp/LispThread  currentThread !()Lorg/armedbear/lisp/LispThread; ! "
   #  	  %  	  ' �(Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/LispObject;)Lorg/armedbear/lisp/LispObject;  )
   * #org/armedbear/lisp/CompiledFunction , <init> JREF / JFLI 1 (ARRAY &REST SUBSCRIPTS) 3 internInPackage A(Ljava/lang/String;Ljava/lang/String;)Lorg/armedbear/lisp/Symbol; 5 6
  7 readObjectFromString 3(Ljava/lang/String;)Lorg/armedbear/lisp/LispObject; 9 :
  ; NIL = 	  > �(Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/LispObject;Lorg/armedbear/lisp/Environment;)V . @
 - A org/armedbear/lisp/Symbol C getSymbolFunctionOrDie !()Lorg/armedbear/lisp/LispObject; E F
 D G Code 
SourceFile jfli-abcl.lisp LineNumberTable !  -    
     
          I   0     $� � � *+� L� $M,� &� (+2+2� +�      .   I   8     ,*02� 84� <� ?� B� 8� &
� 8� H� (�      J    K