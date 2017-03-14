/*=========================================================================*/
/*                    FILE PATH/NAME:   st3_includes/menus.h               */
/*                                                                         */
/*                              HEADER FILE                                */
/*                                                                         */
/*=========================================================================*/

#ifndef MENUS_H
#define MENUS_H

#define DEFAULT_MAX_ROWS 51
#define DEFAULT_MAX_COLS 13
#define DEFAULT_VISIBLE_ROWS 10
#define DEFAULT_VISIBLE_COLS 12
#define EDIT_TEXT_COLOR  "white"

extern  Widget  xs_create_quit_button();
void            v_scrollbar_moved();
void            change_rc();
void            change_val();
void            edit_gage_value();

typedef struct  {
		 Widget     rc, **text_w, **labels, *h_labels;
		 char       **headings,**tdata;
		 float      **data_array;
		 int        num_children;
		 Widget     *gage_table_children;
		 int        first_row_visible, first_col_visible;
		 int        visible_columns, visible_rows;
		 int        maximum_columns, maximum_rows;
		 int        current_column, current_row;
		 int        **changed;
		} cb_struct;

#endif /* #ifndef MENUS_H */
