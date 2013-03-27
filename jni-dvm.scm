#>
#include <jni.h>
<#

(module jni-dvm
        *
        (import chicken scheme foreign srfi-1 data-structures extras)
        (import-for-syntax chicken)
        (use lolevel foreigners srfi-13)

        (include "jni-primitives.scm")
        (include "jni-defs.scm")
        (include "jni-jlambda.scm")
        (include "jni-reflection.scm"))
