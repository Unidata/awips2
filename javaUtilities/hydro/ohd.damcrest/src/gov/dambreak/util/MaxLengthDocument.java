package gov.dambreak.util;

import javax.swing.text.*; 
import java.awt.Toolkit;

/**
 * This custom document is used by the SMPDBK GUI to restrict the length of the
 * DamName, RiverName, and PointOfInterestName text fields.
 */
public class MaxLengthDocument extends DefaultStyledDocument {
    
    int maxCharacters;
    
    public MaxLengthDocument(int maxChars) {
        maxCharacters = maxChars;
    }
    public void insertString(int offs, String str, AttributeSet a) 
    throws BadLocationException 
    {
        
        // This rejects the entire insertion if it would make
        // the contents too long.
        
        if (str == null)
            System.out.println("The text field value is null");
        
        if ((getLength() + str.length()) <= maxCharacters)
            super.insertString(offs, str, a);
        else
            Toolkit.getDefaultToolkit().beep();
    }
}
