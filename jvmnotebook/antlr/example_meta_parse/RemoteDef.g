/**
 * Simple Remote Definition Meta Language
 * (with parsing, more practical usage)
 * Grammar Definition for Antlr (3.0+)
 * Date: 12/27/2007
 * Author: Berlin Brown
 *
 * Description Current Version:
 *
 * Parse the Example Remote Def file and 
 * print the important values.
 *
 * In total, we are only building basic data structures
 * out of the meta language file (lists, maps, etc)
 *
 * Also see, http://javadude.com/articles/antlrtut/
 */

//***********************************************
// Begin Grammar Definitions
//***********************************************
 
grammar RemoteDef;

options {
    k = 2;
}

scope Symbols {
	Map types; // JAVA definition
}


@header {
import java.util.Set;
import java.util.Map;
import java.util.HashSet;
import java.util.HashMap;
}

@members {
	private Map rootNamespaceAttributes = new HashMap();
	private Stack rootOperations = new Stack();
	private boolean buildRootOperationFlag = false;
	
	Map getRootNamespaceAttributes() {
		return rootNamespaceAttributes;		
	}
	
	Stack getRootOperations() {
		return rootOperations;
	}
}


root_meta_declarations 
	scope Symbols; 
	@init {
	  $Symbols::types = new HashMap();
	} : 
	meta_declaration+
	;

meta_declaration :
	root_namespace
	;

/** 
 * Only one root namespace, allowed.
 */
root_namespace :
	OPEN_PAREN ( operation_declaration_list|statement_expression_list|end_root_attr_expression )+ CLOSE_PAREN
	{ 
		// JAVA COMMENT: name space defined.
		System.out.println("INFO: ROOT NAMESPACE FOUND: ");
	}
	;

/**
 * Root expression list
 */
statement_expression_list :
	( attribute_expression )+
	;

end_root_attr_expression : 
	'----'	
	{
		System.out.println("END OF ROOT ATTRIBUTES FOUND");
		buildRootOperationFlag = true;
	}
	;
	
begin_oper_attr_expression : 
	IDENTIFIER_ATOM
	{
		System.out.println("BEGIN OPERATIONS FOUND");
		// JAVA_COMMENT: create a new operation stack.
		rootOperations.push(new HashMap());
	}
	;
	
/**
 * Attributes are defined with  @attr: val;
 * Sub hash data structures are defined by
 * ID { <DATA> }
 *
 * Data payloads (String Content) is enclosed
 * between <<< and >>> tags.
 */
operation_declaration_list :
	( begin_oper_attr_expression OPEN_BRACE ( statement_expression_list | DATA_PAYLOAD_VALUE )+
	CLOSE_BRACE )		
	{		
		// JAVA COMMENT: print the data payload
		if ($DATA_PAYLOAD_VALUE != null) {
			System.out.println("INFO: data payload: [" + $DATA_PAYLOAD_VALUE.text + "]");
			((Map) (rootOperations.peek())).put("data.payload", $DATA_PAYLOAD_VALUE.text);
		} // End if
	}
	;



/**
 * Process a attribute statement expression, which has the following syntax:
 * <code>@some_attribute: some_value;</code>
 */
attribute_expression :
	AT_SIGN_IDENTIFIER attribute_atom_key COLON IDENTIFIER_ATOM END_EXPRESSION
	{
		// JAVA COMMENT: print the attribute key
		System.out.println("INFO: define attribute expr: [" + $IDENTIFIER_ATOM.text + "]");
		System.out.println("INFO: key: " + $attribute_atom_key.text);
		
		$Symbols::types.put($attribute_atom_key.text, $IDENTIFIER_ATOM.text);
		
		// If operations enabled, accumulate that map data
		if (buildRootOperationFlag) { 			
			((Map) (rootOperations.peek())).put($attribute_atom_key.text, $IDENTIFIER_ATOM.text);
		} else {
			getRootNamespaceAttributes().put($attribute_atom_key.text, $IDENTIFIER_ATOM.text);
		}
	}
	;
	
attribute_key :
	AT_SIGN_IDENTIFIER attribute_val COLON
	{ 
		// JAVA COMMENT: print the attribute value
		System.out.println("INFO: define attribute key: [" + $attribute_val.text + "]");
	}
	;
	
attribute_atom_key :
	( IDENTIFIER_ATOM )*
	;	
	
attribute_val :
	( IDENTIFIER_ATOM )*
	;	

//***********************************************
// Misc Utility Definitions and Tokens
//***********************************************

IDENTIFIER_ATOM :	
	( LETTER | '0'..'9' )*
	;
	
fragment LETTER :	
		'$'
	|	'A'..'Z'
	|	'.'
	|	'a'..'z'
	|	'_'
	|	'-'
	;	

DATA_PAYLOAD_VALUE : 
	'<<<' ( . )* '>>>'
	{
		// JAVA COMMENT:
	}
	;

//***********************************************
// Operators
//
// For example, open and close parens are defined
//***********************************************

COLON : ':' ;

END_EXPRESSION : ';' ;

AT_SIGN_IDENTIFIER : '@' ;

OPEN_PAREN : '(' ;

CLOSE_PAREN : ')' ;

OPEN_BRACE : '{' ;

CLOSE_BRACE : '}' ;

//***********************************************
// Ignore (whitespace, comments)
//***********************************************

WS :
	(' '|'\r'|'\t'|'\u000C'|'\n') 
	{channel=99;}
    ;

COMMENT :
	'/*' ( options {greedy=false;} : . )* '*/' {channel=99;}
    ;

LINE_COMMENT : 
	'//' ~('\n'|'\r')* '\r'? '\n' 
	{
		// JAVA COMMENT: set channel = 99
		channel=99;		
	}
    ;

// End of Grammar File //
