(use jni lolevel expand-full moremacros srfi-13 test)
(import-for-syntax jni chicken scheme)

(begin-for-syntax
  (jvm-init-lolevel "tests/test-expansiontime.jar:java/misc-utils/misc-utils.jar"))

(jvm-init-lolevel "tests/test-runtime.jar:java/misc-utils/misc-utils.jar")

(jimport com.chicken_mobile.test.Unestable (prefix <> Unestable-))

(let ((o (Unestable-new)))
  (test-error (Unestable-foo1 o)) ; is available in expansion time jar, but not in runtime
  (test 0 (Unestable-bar o))     ; is available in both jars
  (test-error (Unestable-r o)))
