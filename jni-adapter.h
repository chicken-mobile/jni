#include <jni.h>

jint jvm_create(JavaVM** jvm, void** jenv, char* classpath, char* stack)
{
    JavaVMInitArgs vm_args;
    JavaVMOption options[2];
    options[0].optionString = classpath;
    options[1].optionString = stack;
    vm_args.version = JNI_VERSION_1_6;
    vm_args.options = options;
    vm_args.nOptions = 2;

    return JNI_CreateJavaVM(jvm, jenv, &vm_args);
}

