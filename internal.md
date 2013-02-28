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

