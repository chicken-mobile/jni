#>
#include <jni.h>
<#

(module jni
*

(import chicken scheme foreign srfi-1 data-structures extras srfi-13)
(import-for-syntax chicken)
(use lolevel foreigners)

(include "jni-primitives.scm")
(include "jni-defs.scm")
(include "jni-jlambda.scm")
(include "jni-reflection.scm")

)
