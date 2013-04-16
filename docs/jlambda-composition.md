 How jlambda components intereacts with each other:
--------------------------------------------------
  
  1      |      2      |         3          |          4

jimport ---> jlambda* ---> jlambda-field
                      ---> jlambda-methods ---> jlambda-method*
                                           ---> jlambda-constructor*

1) jimport defines a module (named with the class name) in which a jlambda is defined for each field, for each method and for new.

2) jlambda expands to jlambda-field (if the name match with a class field) or to jlambda-method (if the name matchs to a class method or with the identifier "new").

3) jlambda-methods expands to a group of jlambda-method or jlambda-constructor
