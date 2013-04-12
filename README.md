# Egg documentation

## Documentation

### JNI Bindings

For more information check: [Java Native Interface Specification](http://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/jniTOC.html)

#### Records/Types

    [foreign-type] java-vm

Pointer to JavaVM structure

    [foreign-type] jni-env

Pointer to JNIEnv structure

    [record] jvm-init-args

    [record] jvm-option

### Procedures and macros

#### jvm-init
    [macro] (jvm-init [CLASS-PATH "."])

Initialize jni when chicken is expected to launch the jvm.

#### jni-init
    [macro] (jni-init)

Initialize jni when the jvm launchs chicken.

#### jvm-get-default-init-args
    [procedure] (jvm-get-default-init-args JVM-INIT-ARGS)

Get default init args for the jvm.

#### jvm-create
    [procedure] (jvm-create JAVA-VM JNI-ENV JVM-INIT-ARGS)

Creates a jvm.

#### jvm-destroy
    [procedure] (jvm-destroy JAVA-VM)

Destroy a jvm.

#### jvm-env
    [procedure] (jvm-env JAVA-VM JNI-ENV)

Set output argument JNI-ENV to the current jvm env.

#### jvm-attach-current-thread
    [procedure] (jvm-attach-current-thread JAVA-VM JNI-ENV)

Attaches the current thread to a Java VM. Returns a JNI interface pointer.

#### jvm-detach-current-thread

    [procedure] (jvm-detach-current-thread JAVA-VM)

Detaches the current thread from a Java VM. All Java monitors held by this thread are released. All Java threads waiting for this thread to die are notified. Returns JNI\_OK on success; returns a suitable JNI error code (a negative number) on failure.

### Java to Scheme

#### jlambda
    [macro] (jlambda CLASS [METHOD/FIELD])

- (jlambda CLASS)  => jclass
- (jlambda field)  => jlambda-field
- (jlambda method) => jlambda-methods

#### jlambda-field
    [macro] (jlambda-field MODIFIERS TYPE CLASS FIELD)

Example:
  (let ((user-lastname (jlambda-field () java.lang.String com.testapp.User lastname))) 
    (print (user-lastname user))
    (set! (user-lastname user) "Perez"))

#### jlambda-field*
    [macro] (jlambda-field MODIFIERS TYPE CLASS FIELD)

Same as jlambda-field macro, but avoid class and field checking until the
invocation, this way the jvm is not invoked in expansion time.

#### jlambda-method
    [macro] (jlambda-method MODIFIERS RETURN-TYPE CLASS METHOD-NAME ARGS...) -> lambda

Returns a lambda associated to the java method. Modifiers could by a list of modifiers or #f
Example:

    (jlambda-method #f boolean java.lang.String contains java.lang.CharSequence)

#### jlambda-methods
    [procedure] (jlambda-methods MODIFIERS CLASS-NAME METHOD-NAME ((RETURN-TYPE (ARGS..)) ...)) -> lambda

This procedure will create jlambda-method variants for each signature. When the procedure is invoked will try to 
resolve the overloaded methods as java will do. If there is not enough information for that, type hints information
should be added to help the identification.

Examples:

    (let ((ov1 (jlambda-methods #f 'com.chicken_mobile.jni.test.Bar 'bar
                         '((int . (int)) 
                           (int . (short))
                           (int . (java.lang.String))))))
    (ov1 bar 1 "hola")
    (ov1 bar 2 )
    (ov1 bar (type: short 1)))

#### jlambda-constructor
    [macro] (jlambda-constructor CLASS ARGS...) -> lambda

Returns a lambda associated to the class constructor.

Example:
    (jlambda-constructor java.lang.Integer int))

#### import-java-ns
    [macro] (import-java-ns ((PACKAGE-FROM IMPORT) ...))

IMPORT could be: 

- a class
- a list of classes
- * (all)

Example:

    (import-java-ns ((java.lang *)
                     (java.lang (System String))
                     (com.bevuta.testapp Foo)))
		(class String)
		(class System)
		(class Foo))

#### class
    [macro] (class CLASS-SYMBOL) -> jclass

Returns the associated jclass. Raise error if the class is not found.
Example:

    (class java.lang.String)

#### super-class
    [procedure] (super-class JCLASS) -> jclass

Returns the jclass corresponding to the argument super class.

#### get-object-class
    [procedure] (get-object-class JOBJECT) -> jclass

Returns the jclass corresponding to the argument object.

#### instance-of?
    [procedure] (instance-of? JOBJECT JCLASS)

Check if the object is instance of the class.

#### same-object?
    [procedure] (same-object? JOBJECT JOBJECT)

Check if two objects are the same one.

#### assignable-from?
    [procedure] (assignable-from? JCLASS JCLASS)

Check if a class is assignable from another.

#### define-method
    [macro] (define-method (METHOD_NAME THIS) RETURN-TYPE BODY..)

Implements a native declared method.

Example:
    (define-method (com.bevuta.androidChickenTest.Backend.main backend) void
       ...)

#### References
    [procedure] (new-local-ref JOBJECT)
    [procedure] (delete-local-ref JOBJECT)
    [procedure] (new-global-ref JOBJECT)
    [procedure] (delete-global-ref JOBJECT)
    
#### Exceptions 

Java exceptions are mapped to conditions of the kind '(exn java exception-class)

#### java-exception-message
    [procedure] (java-exception-message JAVA-CONDITION)

Get java exception message as string.

#### java-exception-trace
    [procedure] (java-exception-trace JAVA-CONDITION)

Get java exception trace as string.

#### java-exception-type
    [procedure] (java-exception-type JAVA-CONDITION) -> EXCEPTION-CLASS-SYMBOL

Get java exception class as symbol.

#### Other

##### monitor-enter
    [procedure] (monitor-enter jobject)

Enters the monitor associated with the underlying Java object referred to by obj argument.

##### monitor-exit
    [procedure] (monitor-exit jobject)

Exit the monitor associated with the underlying Java object referred to by obj argument.

