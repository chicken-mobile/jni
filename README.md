# Chicken JNI binding

## Documentation

### JNI Bindings

#### Records/Types

    [foreign-type] java-vm

Pointer to JavaVM structure

    [foreign-type] jni-env

Pointer to JNIEnv structure

    [record] jvm-init-args

    [record] jvm-option

#### Procedures and macros

For more information check: [Java Native Interface Specification](http://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/jniTOC.html)

    [procedure] (jvm-init [CLASS-PATH])

Initialize jni when chicken is expected to launch the jvm, by default CLASS-PATH points to the current directory.

    [macro] (jni-init)

Initialize jni when the jvm launchs chicken.

    [procedure] (jvm-get-default-init-args JVM-INIT-ARGS)

Get default init args for the jvm.

    [procedure] (jvm-create JAVA-VM JNI-ENV JVM-INIT-ARGS)

Creates a jvm.

    [procedure] (jvm-destroy JAVA-VM)

Destroy a jvm.

    [procedure] (jvm-env

    [procedure] (jvm-attach-current-thread

    [procedure] (jvm-detach-current-thread
