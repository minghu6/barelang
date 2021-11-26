use proc_macros::make_simple_error_rules;



make_simple_error_rules!(XXXError);
make_simple_error_rules!(CircularDependencyError);
make_simple_error_rules!(UnknownDefineError);
make_simple_error_rules!(MissMatchedRetTyError);
make_simple_error_rules!(IllegalFunctionCallError);
make_simple_error_rules!(UnknownGenericNameError);
make_simple_error_rules!(UnmatchedGenericNumberError);
make_simple_error_rules!(UnmatchedFunctionArgsNumberError);

make_simple_error_rules!(VoidAsArgError);
make_simple_error_rules!(UnknownFunctionError);
make_simple_error_rules!(UnknownVarError);
make_simple_error_rules!(UnknownFieldError);
make_simple_error_rules!(UnknownTemplateStructError);
make_simple_error_rules!(UnknownStructError);
make_simple_error_rules!(UnrecognizedPrimaryTypeNameError);
make_simple_error_rules!(MalformedSyntaxError);
make_simple_error_rules!(NonPrimitiveCompareError);
make_simple_error_rules!(ExplicitCastRequiredError);
make_simple_error_rules!(UnsupportedIntMathOpError);
make_simple_error_rules!(UnsupportedFloatMathOpError);
make_simple_error_rules!(UnsupportedNumberMathOpError);
make_simple_error_rules!(UnsupportedOpTypeError);

