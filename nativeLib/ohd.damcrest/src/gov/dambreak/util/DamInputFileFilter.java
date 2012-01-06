package gov.dambreak.util;

import java.io.File;

/**
 * This file filter is used by the GUI when creating a JFileChooser().  It
 * restricts the files the user is allowed to choose .DAT, .OUT, .DAM or .XY
 * or .TXT files.
 */
public class DamInputFileFilter extends javax.swing.filechooser.FileFilter 
{
  public static int DAT_INPUT = 0;
  public static int OUTPUT = 1;
  public static int DAM_INPUT = 2;
  public static int FLDVIEW_OUTPUT = 3;
  public static int TXT_INPUT = 4;
  private int nType;
  /**
    This is the one of the methods that is declared in 
    the abstract class
   */
	public DamInputFileFilter(int _nType) {
		nType = _nType;
	}
  public boolean accept(File f) 
  {
    //if it is a directory -- we want to show it so return true.
    if (f.isDirectory()) 
      return true;
  
    //get the extension of the file
    String extension = getExtension(f);
    
    //check to see if the extension is equal to "dat", "out", or "dam"
    
    if (nType == 0 && extension.equals("dat")) 
		return true; 
    if (nType == 1 && extension.equals("out")) 
		return true;
    if (nType == 2 && extension.equals("dam"))
    	return true;

    if (nType == 3 && extension.equals("xy"))
    	return true;

    if (nType == 4 && extension.equals("txt")) 
		return true; 
    return false;
  }
  /**
    Again, this is declared in the abstract class

    The description of this filter
   */
  public String getDescription() 
  {
		if (nType == 0)
			return "SMPDBK Input files (*.DAT)";
		else if (nType == 1)
			return "SMPDBK Output files (*.OUT)";
		else if (nType == 2)
			return "Dambreak Analysis files (*.DAM)";
		else if (nType == 3)
			return "FLDVIEW files (*.XY)";
		else if (nType == 4)
			return "FLDXS cross section files (*.TXT)";
		else
			return "Unrecognized filter";
  }
  /**
    Method to get the extension of the file, in lowercase
   */
  private String getExtension(File f) 
  {
    String s = f.getName();
    int i = s.lastIndexOf('.');
    if (i > 0 &&  i < s.length() - 1) 
      return s.substring(i+1).toLowerCase();
    return "";
  }
}
