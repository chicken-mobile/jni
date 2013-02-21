#>
#include <jni.h>
<#

(module jni
*

(import chicken scheme foreign)
(import-for-syntax chicken data-structures)
(use lolevel foreigners)

(include "jni-primitives.scm")
(include "jni-defs.scm")
(include "jni-jlambda.scm")
(include "jni-java-utils.scm")

)
