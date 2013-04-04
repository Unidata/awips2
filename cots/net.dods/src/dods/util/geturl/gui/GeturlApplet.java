/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1998, California Institute of Technology.  
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged. 
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Jake Hamby, NASA/Jet Propulsion Laboratory
//         Jake.Hamby@jpl.nasa.gov
/////////////////////////////////////////////////////////////////////////////

package dods.util.geturl.gui;
import java.applet.*;
import java.awt.*;
import java.awt.event.*;

/** Java geturl applet. */
public class GeturlApplet extends Applet {
  /** The currently open Geturl window. */
  protected GeturlFrame frame;

  /** Open the frame and add the start button to the layout */
  public void init() {
    frame = new GeturlFrame(true);
    Button restartButton = new Button("Restart");
    restartButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
	frame.dispose();
	frame = new GeturlFrame(true);
      }
    });
    add(restartButton);
  }

  /** Dispose of the frame */
  public void dispose() {
    frame.dispose();
  }

  /** Main function to call as an application. */
  public static void main(String args[]) {
    GeturlFrame frame = new GeturlFrame(false);
  }
}
