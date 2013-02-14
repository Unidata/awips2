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

import dods.util.gui.msg_box;
import dods.util.gui.warning_box;
import dods.util.gui.error_box;

/**
 * Test routine for the dods/util/gui classes
 *
 * @version $Revision: 1.3 $
 * @author ndp
 */

public class GuiTest {


    // Constructor
    public GuiTest() {
    }


    public static void main(String[] args){

	msg_box mbox = new msg_box("Wow! A Message Box!");
	msg_box mbox1 = new msg_box("My Title1",
				"Wow! A Message Box! Maybe I should test a really long string in it to see if it automatically resizes for the text contained within it?\nOk!\nLets do That!");

	warning_box wbox = new warning_box("Wow! A Warning Message Box!");
	warning_box wbox1 = new warning_box("My Title2",
				"Wow! A Warning Message Box!");

	error_box ebox = new error_box("Wow! A Error Message Box!");
	error_box ebox1 = new error_box("My Title3",
				"Wow! A Error Message Box!");

	System.exit(0);
    }


}

