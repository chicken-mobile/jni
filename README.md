# Chicken JNI bindings

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
    [procedure] (jvm-init [CLASS-PATH])

Initialize jni when chicken is expected to launch the jvm, by default CLASS-PATH points to the current directory.

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

#### jlambda-method
    [macro] (jlambda-method MODIFIERS CLASS RETURN-TYPE METHOD-NAME ARGS...) -> lambda

Returns a lambda associated to the java method.

#### get-method-id
    [procedure] (get-method-id JCLASS name signature) -> jmethod-id

    (get-method-id jclass "<init>" "()V")

#### method
    [macro] (method CLASS-SYMBOL RETURN-TYPE-SYMBOL ARGS-PROTOTYPE)

Example:
    (method java.lang.String void <init>)

#### get-field
    [procedure] (get-field JCLASS name signature)

#### get-static-field
    [procedure] (get-static-field JCLASS name signature)

#### get-static-method-id
    [procedure] (get-static-method-id JCLASS name signature)

#### class
    [macro] (class CLASS-SYMBOL) -> jclass

Returns the associated jclass.

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

#### new-object
    [procedure] (new-object JCLASS JMETHOD-ID) -> jobject

Returns a new allocated object, eg:
    
    (let* ((jclass        (class java.lang.String))
           (constructor   (get-method-id jclass "<init>" "()V")))
        (new-object jclass constructor))

#### constructor
    [macro] (constructor CLASS-SYMBOL [ARGS..])

Convenient wrap for new-object.

#### References
    [procedure] (new-local-ref JOBJECT)
    [procedure] (delete-local-ref JOBJECT)
    [procedure] (new-global-ref JOBJECT)
    [procedure] (delete-global-ref JOBJECT)
    
#### make-array
    [procedure] (make-array size JCLASS JOBJECT) -> jarray

#### array-length
    [procedure] (array-length J-ARRAY)

#### array-ref
    [procedure] (array-ref J-ARRAY size) -> jobject

#### array-set!
    [procedure] (array-set! J-ARRAY index JOBJECT)

#### exception-check
    [procedure] (exception-check)

Check for pending exceptions.

#### exception-clear
    [procedure] (exception-clear)

Clear pending exceptions.

#### exception-describe
    [procedure] (exception-describe)

Prints an exception and a backtrace of the stack to a system error-reporting channel, such as stderr. This is a convenience routine provided for debugging.

#### exception-occurred
    [procedure] (exception-occurred) -> jthrowable

Determines if an exception is being thrown. The exception stays being thrown
until either the native code calls ExceptionClear().

#### monitor-enter
    [procedure] (monitor-enter jobject)

Enters the monitor associated with the underlying Java object referred to by obj argument.

#### monitor-exit
    [procedure] (monitor-exit jobject)

Exit the monitor associated with the underlying Java object referred to by obj argument.

