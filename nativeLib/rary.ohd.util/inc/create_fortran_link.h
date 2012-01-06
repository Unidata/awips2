/*
 * create_fortran_link.h
 *
 *  Created on: May 5, 2009
 *      Author: jelkins
 */

#ifndef CREATE_FORTRAN_LINK_H_
#define CREATE_FORTRAN_LINK_H_

#define create_fortran_link(function_return,c_function_name,arguments_with_type,arguments_without_type)\
	function_return c_function_name arguments_with_type;\
	function_return c_function_name ##_ arguments_with_type  { c_function_name arguments_without_type; }\
    function_return c_function_name arguments_with_type


#endif /* CREATE_FORTRAN_LINK_H_ */
