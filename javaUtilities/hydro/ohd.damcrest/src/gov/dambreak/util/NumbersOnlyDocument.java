package gov.dambreak.util;

import javax.swing.text.*; 
import java.awt.Toolkit;

/**
 * Derived Class 
 * Creation date: (7/15/2003 1:07:45 PM)
 * @author: 
 */
public class NumbersOnlyDocument extends DefaultStyledDocument 
{
    /**
     * NumbersOnlyDocument constructor comment.
     */
    public NumbersOnlyDocument() 
    {
        super();
    } 
    /**
     * NumbersOnlyDocument constructor comment.
     * @param c javax.swing.text.AbstractDocument.Content
     * @param styles javax.swing.text.StyleContext
     */
    public NumbersOnlyDocument(javax.swing.text.AbstractDocument.Content c, javax.swing.text.StyleContext styles) 
    {
        super(c, styles);
    }
    /**
     * NumbersOnlyDocument constructor comment.
     * @param styles javax.swing.text.StyleContext
     */
    public NumbersOnlyDocument(javax.swing.text.StyleContext styles) 
    {
        super(styles);
    }
    /**
     * Insert the method's description here.
     * Creation date: (7/15/2003 1:08:41 PM)
     * @param offs int
     * @param str java.lang.String
     * @param a javax.swing.text.AttributeSet
     */
    public void insertString(int offs, String str, AttributeSet a) 
    throws BadLocationException 
    {
        
        // This rejects the entire insertion if it causes
        // the text to not remain a number.
        String strResult = getText(0,offs) + str + getText(offs,getLength()-offs);
        
        if (strResult.endsWith("f")) 
        {
            Toolkit.getDefaultToolkit().beep();
            return;
        }
        
        if (getLength() == 0 && str.length() == 1 && (str.charAt(0) == '-' || str.charAt(0) == '+' || str.charAt(0) == '.'))
        {
            super.insertString(offs, str, a);
            return;
        }
        try 
        {
            if (!str.equals(""))
                Double.parseDouble(strResult);
            super.insertString(offs, str, a);
        } 
        catch (NumberFormatException e) {
            System.out.println("!" + str + "!");
            Toolkit.getDefaultToolkit().beep();
        }
    }
}
