/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1999, COAS, Oregon State University  
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged. 
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Nathan Potter (ndp@oce.orst.edu)
//
//                        College of Oceanic and Atmospheric Scieneces
//                        Oregon State University
//                        104 Ocean. Admin. Bldg.
//                        Corvallis, OR 97331-5503
//         
/////////////////////////////////////////////////////////////////////////////

package dods.util.gui;

import javax.swing.*;

/**
 * Dispays a simple warning message box.
 *
 * @version $Revision: 1.2 $
 * @author ndp
 */

public class warning_box {


    /** Construct and display a warning dialog box.
	@param msg This string is used as the message text in the dialog box.
    */
    public warning_box(String msg) {

	JLabel jl = new JLabel(msg);

	JOptionPane.showMessageDialog(	null,	// parent frame
					jl,	// Object to Display
					"WARNING!", // Title bar label
					JOptionPane.WARNING_MESSAGE );
    }

    /** Construct and display a warning dialog box.
	@param title This String is used in the title bar of the dialog box.
	@param msg This string is used as the message text in the dialog box.
    */
    public warning_box(String title, String msg) {

	JLabel jl = new JLabel(msg);

	JOptionPane.showMessageDialog(	null,	// parent frame
					jl,	// Object to Display
					title, // Title bar label
					JOptionPane.WARNING_MESSAGE );
    }

}

