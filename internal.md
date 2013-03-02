# Chicken JNI bindings

## Documentation

### Java to Scheme

#### get-method-id
    [procedure] (get-method-id JCLASS name signature) -> jmethod-id

Example:

    (get-method-id jclass "<init>" "()V")

#### method
    [macro] (method CLASS-SYMBOL RETURN-TYPE-SYMBOL ARGS-PROTOTYPE)

Convenient macro to handle get-method-id

Example:

    (method java.lang.String void <init>)

#### get-field
    [procedure] (get-field JCLASS name signature)

#### get-static-field
    [procedure] (get-static-field JCLASS name signature)

#### get-static-method-id
    [procedure] (get-static-method-id JCLASS name signature)

#### Arrays

##### make-array
    [procedure] (make-array size JCLASS JOBJECT) -> jarray

##### array-length
    [procedure] (array-length J-ARRAY)

##### array-ref
    [procedure] (array-ref J-ARRAY size) -> jobject

##### array-set!
    [procedure] (array-set! J-ARRAY index JOBJECT)

#### new-object
    [procedure] (new-object JCLASS JMETHOD-ID) -> jobject

Returns a new allocated object, eg:
    
Example:

    (let* ((string-class (class java.lang.String))
           (constructor   (get-method-id string-class "<init>" "()V")))
        (new-object string-class constructor))

#### constructor
    [macro] (constructor CLASS-SYMBOL [ARGS..])

Convenient wrap for get-method-id for constructor methods.

Example:

    (let ((string-class (class java.lang.String))
          (c (constructor java.lang.String)))
        (new-object string-class c))

