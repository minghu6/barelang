use proc_macros::make_simple_error_rules;



make_simple_error_rules!(XXXError);
make_simple_error_rules!(CircularDependencyError);
make_simple_error_rules!(UnknownDefineError);
make_simple_error_rules!(MissMatchedRetTyError);
make_simple_error_rules!(IllegalFunctionCallError);
make_simple_error_rules!(UnknownGenericNameError);
make_simple_error_rules!(VoidAsArgError);
make_simple_error_rules!(UnknownFunctionError);
make_simple_error_rules!(UnknownVarError);