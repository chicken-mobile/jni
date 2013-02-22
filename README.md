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

Detaches the current thread from a Java VM. All Java monitors held by this thread are released. All Java threads waiting for this thread to die are notified. Returns JNI_OK on success; returns a suitable JNI error code (a negative number) on failure.

### Java to Scheme

#### jlambda-method
    [macro] (jlambda-method MODIFIERS CLASS RETURN-TYPE METHOD-NAME ARGS...)

Returns a lambda associated to the java method.
