#>
#include <jni.h>
<#

(module jni
*

(import chicken scheme foreign srfi-1 data-structures extras srfi-13)
(import-for-syntax chicken)
(use lolevel foreigners srfi-1)

(include "jni-primitives.scm")
(include "jni-defs.scm")
(include "jni-jlambda.scm")
(include "jni-import-ns.scm")
(include "jni-reflection.scm")

)
