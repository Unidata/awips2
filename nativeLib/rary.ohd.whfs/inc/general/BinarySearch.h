#ifndef BINARYSEARCH_H
#define BINARYSEARCH_H

typedef int ( * CompareFunction ) ( void * search_value , 
                                    void * array_value ) ;

void * binary_search ( void * pArray ,
                       void * search_value ,
                       int num_items ,
                       size_t size_of_array_element ,
                       CompareFunction compare_function ) ;

int get_result_index ( );

#endif /* #ifndef BINARYSEARCH_H */
