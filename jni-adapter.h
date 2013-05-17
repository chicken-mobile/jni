#include <jni.h>

char* string_to_pointer(char* string){
  printf(string, "\n");
  return string;
}

jint jvm_create(JavaVM** jvm, void** jenv, void** pointerVector, uint length)
{
  JavaVMInitArgs vm_args;
  JavaVMOption options[length];

  vm_args.version = JNI_VERSION_1_6;

  static int i;
  for(i = 0; i < length; i++){
    options[i].optionString = *(((char**)pointerVector) + i);
  }

  vm_args.options = options;
  vm_args.nOptions = length;


  return JNI_CreateJavaVM(jvm, jenv, &vm_args);
}
