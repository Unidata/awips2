/*****************************************************************************
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of the HDF Java Products distribution.                  *
 * The full copyright notice, including terms governing use, modification,   *
 * and redistribution, is contained in the files COPYING and Copyright.html. *
 * COPYING can be found at the root of the source code distribution tree.    *
 * Or, see http://hdfgroup.org/products/hdf-java/doc/Copyright.html.         *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 ****************************************************************************/

package ncsa.hdf.view;

import java.awt.event.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.border.TitledBorder;

import ncsa.hdf.object.HObject;

import java.awt.Point;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.lang.reflect.Array;

/**
 * MathConversionDialog shows a message dialog requesting user input for
 * math conversion.
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class MathConversionDialog extends JDialog
implements ActionListener, ListSelectionListener
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    private JTextField aField, bField;

    private JTextArea infoArea;

    private JList functionList;

    private Object dataValue;

    private char NT;

    private final Toolkit toolkit;

    private String[] functionDescription;

    private boolean isConverted;

    /** Constructs MathConversionDialog.
     *  @param owner the owner of the input
     *  @param data the data array to convert.
     */
    public MathConversionDialog(JFrame owner, Object data)
    {
        super (owner, "Convert Data...", true);

        toolkit = Toolkit.getDefaultToolkit();
        isConverted = false;
        dataValue = data;
        NT = ' ';

        String cName = data.getClass().getName();
        int cIndex = cName.lastIndexOf("[");
        if (cIndex >= 0 ) {
            NT = cName.charAt(cIndex+1);
        }

        String[] functionNames = {
            "[a, b]",
            "abs (x)",
            "a + b * x",
            "pow (x, a)",
            "exp (x)",
            "ln (x)",
            "log (a, x)",
            "sin (x)",
            "cos (x)",
            "tan (x)"};
        functionList = new JList(functionNames);
        functionList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        functionList.addListSelectionListener(this);

        String[] tmpStrs = {
            "The filter by lower and upper bounds. x=a if x<a; x=b if x>b."+
                "\ne.g.\n x=5, [0, 127]=5\n x=-5, [0, 127]=0\n x=255, [0, 127]=127.",
            "The absolute value of a number, the number without its sign."+
                "\ne.g.\n abs(5)=5\n abs(-5)=5.",
            "Linear function."+
                "\ne.g.\n a=5, b=2, x=2.5, a+b*x=10.",
            "The result of a number raised to power of a."+
                "\ne.g.\n x=2.5, a=10, pow(x, a)=9536.743\n x=25, a=0.5, pow(x, a)=5.",
            "The exponential number e (i.e., 2.718...) raised to the power of x."+
                "\ne.g.\n exp(5.0)=148.41316\n exp(5.5)=244.69193",
            "The natural logarithm (base e) of x."+
                "\ne.g.\n ln(20.085541)=3\n ln(10)=2.302585",
            "The logarithm of x to the base of a, \"a\" must be an integer > 0."+
                "\ne.g.\n log(10, 2)=3.321928\n log(2, 10)=0.30103",
            "The trigonometric sine of angle x in radians."+
                "\ne.g.\n sin(0.523599)=0.5\n sin(1.047198)=0.866025",
            "The trigonometric cosine of angle x in radians."+
                "\ne.g.\n cos(0.523599)=0.866025\n cos(1.047198)=0.5",
            "The trigonometric tangent of angle x in radians."+
                "\ne.g.\n tan(0.785398)=1\n tan(1.047198)=1.732051"};

        functionDescription = tmpStrs;

        JPanel contentPane = (JPanel)getContentPane();
        contentPane.setLayout(new BorderLayout(5,5));
        contentPane.setBorder(BorderFactory.createEmptyBorder(10,5,5,5));
        int w = 500 + (ViewProperties.getFontSize()-12)*15;
        int h = 300 + (ViewProperties.getFontSize()-12)*10;
        contentPane.setPreferredSize(new Dimension(w, h));

        JButton okButton = new JButton("   Ok   ");
        okButton.setActionCommand("Ok");
        okButton.setMnemonic(KeyEvent.VK_O);
        okButton.addActionListener(this);

        JButton cancelButton = new JButton("Cancel");
        cancelButton.setMnemonic(KeyEvent.VK_C);
        cancelButton.setActionCommand("Cancel");
        cancelButton.addActionListener(this);

        // set OK and CANCEL buttons
        JPanel buttonPanel = new JPanel();
        buttonPanel.add(okButton);
        buttonPanel.add(cancelButton);
        contentPane.add(buttonPanel, BorderLayout.SOUTH);

        // set name, parent, width and height panel
        JPanel centerP = new JPanel();
        centerP.setLayout(new BorderLayout(10, 10));
        JScrollPane scroller = new JScrollPane(functionList);
        centerP.add(scroller, BorderLayout.CENTER);

        JPanel tmpP = new JPanel();
        tmpP.setLayout(new BorderLayout(5,5));

        JPanel tmpP0 = new JPanel();
        tmpP0.setLayout(new GridLayout(4,1,5,5));
        tmpP0.add(new JLabel("a = "));
        tmpP0.add(new JLabel("b = "));
        tmpP0.add(new JLabel("                     "));
        tmpP0.add(new JLabel("                     "));
        tmpP.add(tmpP0, BorderLayout.WEST);

        tmpP0 = new JPanel();
        tmpP0.setLayout(new GridLayout(4,1,5,5));
        tmpP0.add(aField = new JTextField("0"));
        tmpP0.add(bField = new JTextField("1"));
        tmpP0.add(new JLabel("                     "));
        tmpP0.add(new JLabel("                     "));
        tmpP.add(tmpP0, BorderLayout.CENTER);

        centerP.add(tmpP, BorderLayout.EAST);

        centerP.setBorder(new TitledBorder("Converting Data With A Mathematic Function"));
        centerP.add(infoArea = new JTextArea(4, 80), BorderLayout.SOUTH);
        infoArea.setEditable(false);
        infoArea.setLineWrap(true);
        infoArea.setBackground(java.awt.Color.lightGray);
        infoArea.setWrapStyleWord(true);
        aField.setEnabled(false);
        bField.setEnabled(false);

        contentPane.add(centerP, BorderLayout.CENTER);

        // locate the H5Property dialog
        Point l = owner.getLocation();
        l.x += 250;
        l.y += 80;
        setLocation(l);
        validate();
        pack();
    }

    private boolean convertData()
    {
        double a=0, b=1;

        int index = functionList.getSelectedIndex();
        try
        {
            if ((index==0) || (index == 2))
            {
                a = Double.parseDouble(aField.getText().trim());
                b = Double.parseDouble(bField.getText().trim());
            }
            else if (index == 3)
            {
                a = Double.parseDouble(aField.getText().trim());
            }
            else if (index == 6)
            {
                a = Integer.parseInt(aField.getText().trim());
                if (a <=0)
                {
                    toolkit.beep();
                    JOptionPane.showMessageDialog(this,
                        "a must be an integer greater than zero.",
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
        } catch (Exception ex)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                ex.getMessage(),
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        int n = Array.getLength(dataValue);
        double value=0, x=0;

        switch (NT)
        {
            case 'B':
                byte[] bdata = (byte[])dataValue;
                for (int i=0; i<n; i++)
                {
                    x = bdata[i];
                    value = y(index, x, a, b);
                    if ((value > Byte.MAX_VALUE) || (value < Byte.MIN_VALUE))
                    {
                        JOptionPane.showMessageDialog(this,
                            "Invalid byte value: "+(long)value,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                        return false;
                    }

                    bdata[i] = (byte)value;
                } //for (int i=0; i<n; i++)
                break;
            case 'S':
                short[] sdata = (short[])dataValue;
                for (int i=0; i<n; i++)
                {
                    x = sdata[i];
                    value = y(index, x, a, b);
                    if ((value > Short.MAX_VALUE) || (value < Short.MIN_VALUE))
                    {
                        JOptionPane.showMessageDialog(this,
                            "Invalid short value: "+(long)value,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                        return false;
                    }

                    sdata[i] = (short)value;
                } //for (int i=0; i<n; i++)
                break;
            case 'I':
                int[] idata = (int[])dataValue;
                for (int i=0; i<n; i++)
                {
                    x = idata[i];
                    value = y(index, x, a, b);
                    if ((value > Integer.MAX_VALUE) || (value < Integer.MIN_VALUE))
                    {
                        JOptionPane.showMessageDialog(this,
                            "Invalid int value: "+(long)value,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                        return false;
                    }

                    idata[i] = (int)value;
                } //for (int i=0; i<n; i++)
                break;
            case 'J':
                long[] ldata = (long[])dataValue;
                for (int i=0; i<n; i++)
                {
                    x = ldata[i];
                    value = y(index, x, a, b);
                    if ((value > Long.MAX_VALUE) || (value < Long.MIN_VALUE))
                    {
                        JOptionPane.showMessageDialog(this,
                            "Invalid long value: "+(long)value,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                        return false;
                    }

                    ldata[i] = (long)value;
                } //for (int i=0; i<n; i++)
                break;
            case 'F':
                float[] fdata = (float[])dataValue;
                for (int i=0; i<n; i++)
                {
                    x = fdata[i];
                    value = y(index, x, a, b);
                    if ((value > Float.MAX_VALUE) || (value < -Float.MAX_VALUE) || (value == Float.NaN))
                    {
                        JOptionPane.showMessageDialog(this,
                            "Invalid float value: "+value,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                        return false;
                    }

                    fdata[i] = (float)value;
                } //for (int i=0; i<n; i++)
                break;
            case 'D':
                double[] ddata = (double[])dataValue;
                for (int i=0; i<n; i++)
                {
                    x = ddata[i];
                    value = y(index, x, a, b);
                    if ((value > Double.MAX_VALUE) || (value < -Double.MAX_VALUE) || (value == Double.NaN))
                    {
                        JOptionPane.showMessageDialog(this,
                            "Invalid double value: "+value,
                            getTitle(),
                            JOptionPane.ERROR_MESSAGE);
                        return false;
                    }

                    ddata[i] = value;
                } //for (int i=0; i<n; i++)
                break;
            default:
                break;
        }

        return true;
    }

    public void actionPerformed(ActionEvent e)
    {
        Object source = e.getSource();
        String cmd = e.getActionCommand();

        if (cmd.equals("Ok"))
        {
            isConverted = convertData();
            //if (isConverted)
                dispose();
        }
        if (cmd.equals("Cancel"))
        {
            isConverted = false;
            dispose();
        }
    }

    public void valueChanged(ListSelectionEvent e)
    {
        if (e.getValueIsAdjusting()) {
            return;
        }

        if (!e.getSource().equals(functionList)) {
            return;
        }

        if (functionList.isSelectionEmpty()) {
            return;
        }

        int index = functionList.getSelectedIndex();
        infoArea.setText(functionDescription[index]);

        if ((index==0) || (index == 2))
        {
            aField.setEnabled(true);
            bField.setEnabled(true);
        }
        else if ((index == 3) || (index == 6))
        {
            aField.setEnabled(true);
            bField.setEnabled(false);
        }
        else
        {
            aField.setEnabled(false);
            bField.setEnabled(false);
        }
    }

    private double y(int index, double x, double a, double b)
    {
        double y = x;
        switch (index)
        {
            case 0: if (x < a) {
                y = a;
            } else if (x > b) {
                y = b;
            } break;
            case 1: y = Math.abs(x);  break;
            case 2: y = (a+b*x); break;
            case 3: y = Math.pow(x, a); break;
            case 4: y = Math.exp(x); break;
            case 5: y = Math.log(x); break;
            case 6: y = (Math.log(x)/Math.log(a)); break;
            case 7: y = Math.sin(x); break;
            case 8: y = Math.cos(x); break;
            case 9: y = Math.tan(x); break;
            default: y = x; break;
        }

        return y;
    }

    /** Returns true if the data is successfully converted. */
    public boolean isConverted()
    {
        return isConverted;
    }

}
