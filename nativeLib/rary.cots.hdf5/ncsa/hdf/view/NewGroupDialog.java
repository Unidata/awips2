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
import java.awt.Frame;
import java.awt.Point;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.util.*;
import ncsa.hdf.object.*;


/**
 * NewGroupDialog shows a message dialog requesting user input for creating
 * a new HDF4/5 group.
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class NewGroupDialog extends JDialog
implements ActionListener
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    private JTextField nameField;

    private JComboBox parentChoice;

    /** a list of current groups */
    private List groupList;

    private HObject newObject;

    private FileFormat fileFormat;

    private final Toolkit toolkit;


    /** Constructs NewGroupDialog with specified list of possible parent groups.
     *  @param owner the owner of the input
     *  @param pGroup the parent group which the new group is added to.
     *  @param objs the list of all objects.
     */
    public NewGroupDialog(Frame owner, Group pGroup, List objs)
    {
        super (owner, "New Group...", true);

        newObject = null;

        fileFormat = pGroup.getFileFormat();
        toolkit = Toolkit.getDefaultToolkit();

        parentChoice = new JComboBox();
        groupList = new Vector();
        Object obj = null;
        Iterator iterator = objs.iterator();
        while (iterator.hasNext())
        {
            obj = iterator.next();
            if (obj instanceof Group)
            {
                groupList.add(obj);
                Group g = (Group)obj;
                if (g.isRoot()) {
                    parentChoice.addItem(HObject.separator);
                } else {
                    parentChoice.addItem(g.getPath()+g.getName()+HObject.separator);
                }
            }
        }

        if (pGroup.isRoot()) {
            parentChoice.setSelectedItem(HObject.separator);
        } else {
            parentChoice.setSelectedItem(pGroup.getPath()+pGroup.getName()+HObject.separator);
        }

        JPanel contentPane = (JPanel)getContentPane();
        contentPane.setLayout(new BorderLayout(5,5));
        contentPane.setBorder(BorderFactory.createEmptyBorder(15,5,5,5));
        int w = 400 + (ViewProperties.getFontSize()-12)*15;
        int h = 120 + (ViewProperties.getFontSize()-12)*10;
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

        // set NAME and PARENT GROUP panel
        JPanel namePanel = new JPanel();
        namePanel.setLayout(new BorderLayout(5,5));
        JPanel tmpP = new JPanel();
        tmpP.setLayout(new GridLayout(2,1));
        tmpP.add(new JLabel("Group name: "));
        tmpP.add(new JLabel("Parent group: "));
        namePanel.add(tmpP, BorderLayout.WEST);
        tmpP = new JPanel();
        tmpP.setLayout(new GridLayout(2,1));
        tmpP.add(nameField=new JTextField());
        tmpP.add(parentChoice);
        namePanel.add(tmpP, BorderLayout.CENTER);
        contentPane.add(namePanel, BorderLayout.CENTER);

        // locate the H5Property dialog
        Point l = owner.getLocation();
        l.x += 250;
        l.y += 80;
        setLocation(l);
        validate();
        pack();
    }

    public void actionPerformed(ActionEvent e)
    {
        Object source = e.getSource();
        String cmd = e.getActionCommand();

        if (cmd.equals("Ok"))
        {
            newObject = create();
            if (newObject != null) {
                dispose();
            }
        }
        if (cmd.equals("Cancel"))
        {
            newObject = null;
            dispose();
        }
    }

    private HObject create()
    {
        String name = null;
        Group pgroup = null;

        name = nameField.getText();
        if (name == null)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Group name is not specified.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        if (name.indexOf(HObject.separator) >= 0)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Group name cannot contain path.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        pgroup = (Group)groupList.get(parentChoice.getSelectedIndex());

        if (pgroup == null)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Parent group is null.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        Group obj = null;
        try
        {
            obj = fileFormat.createGroup(name, pgroup);
        } catch (Exception ex)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                ex.getMessage(),
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        return obj;
    }

    /** Returns the new group created. */
    public DataFormat getObject() { return newObject; }

    /** Returns the parent group of the new group. */
    public Group getParentGroup() {
        return (Group)groupList.get(parentChoice.getSelectedIndex());
    }

}
