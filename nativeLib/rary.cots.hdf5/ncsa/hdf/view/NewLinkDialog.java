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
import java.awt.Point;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.util.*;

import ncsa.hdf.object.*;

/**
 * NewDatasetDialog shows a message dialog requesting user input for creating
 * a new HDF4/5 dataset.
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class NewLinkDialog extends JDialog
implements ActionListener
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    private JTextField nameField;

    private JComboBox parentChoice, linkToChoice;

    private JCheckBox checkUnsigned;

    /** a list of current groups */
    private List groupList;

    /** a list of current objects */
    private List objList;

    private HObject newObject;

    private FileFormat fileFormat;

    private final Toolkit toolkit;

    /** Constructs NewLinkDialog with specified list of possible parent groups.
     *  @param owner the owner of the input
     *  @param pGroup the parent group which the new group is added to.
     *  @param objs the list of all objects.
     */
    public NewLinkDialog(JFrame owner, Group pGroup, List objs)
    {
        super (owner, "New Link...", true);

        newObject = null;

        fileFormat = pGroup.getFileFormat();
        toolkit = Toolkit.getDefaultToolkit();
        objList = objs;

        parentChoice = new JComboBox();
        linkToChoice = new JComboBox();

        groupList = new Vector(objs.size());
        HObject obj = null;
        Iterator iterator = objs.iterator();
        String full_name = null;
        int idx_root=-1, idx=-1;
        while (iterator.hasNext())
        {
            obj = (HObject)iterator.next();
            idx++;

            if (obj instanceof Group)
            {
                Group g = (Group)obj;
                groupList.add(obj);
                if (g.isRoot())
                {
                    full_name = HObject.separator;
                    idx_root = idx;
                } else {
                    full_name = g.getPath()+g.getName()+HObject.separator;
                }
                parentChoice.addItem(full_name);
            } else {
                full_name = obj.getPath()+obj.getName();
            }

            linkToChoice.addItem(full_name);
        }

        linkToChoice.removeItemAt(idx_root);
        objList.remove(idx_root);

        if (pGroup.isRoot()) {
            parentChoice.setSelectedItem(HObject.separator);
        } else {
            parentChoice.setSelectedItem(pGroup.getPath()+pGroup.getName()+HObject.separator);
        }

        JPanel contentPane = (JPanel)getContentPane();
        contentPane.setLayout(new BorderLayout(5,5));
        contentPane.setBorder(BorderFactory.createEmptyBorder(15,5,5,5));
        int w = 400 + (ViewProperties.getFontSize()-12)*15;
        int h = 150 + (ViewProperties.getFontSize()-12)*10;
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
        tmpP.setLayout(new GridLayout(3,1));
        tmpP.add(new JLabel("Link name: "));
        tmpP.add(new JLabel("Parent group: "));
        tmpP.add(new JLabel("Link to: "));
        namePanel.add(tmpP, BorderLayout.WEST);
        tmpP = new JPanel();
        tmpP.setLayout(new GridLayout(3,1));
        tmpP.add(nameField=new JTextField());
        tmpP.add(parentChoice);
        tmpP.add(linkToChoice);
        namePanel.add(tmpP, BorderLayout.CENTER);
        contentPane.add(namePanel, BorderLayout.CENTER);

        // locate the H5Property dialog
        Point l = owner.getLocation();
        l.x += 250;
        l.y += 100;
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
            newObject = createLink();

            if (newObject != null) {
                dispose();
            }
        }
        if (cmd.equals("Cancel"))
        {
            newObject = null;
            dispose();
            ((Vector)groupList).setSize(0);
        }
    }

    private HObject createLink()
    {
        String name = null;
        Group pgroup = null;

        name = nameField.getText().trim();
        if ((name == null) || (name.length()<1))
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Dataset name is not specified.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        if (name.indexOf(HObject.separator) >= 0)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Dataset name cannot contain path.",
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

        HObject currentObj = (HObject)objList.get(linkToChoice.getSelectedIndex());

        if (currentObj == null)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Target object is null.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        if ((currentObj instanceof Group) && ((Group)currentObj).isRoot())
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Cannot make a link to the root group.",
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        HObject obj = null;
        try
        {
            obj = fileFormat.createLink(pgroup, name, currentObj);
        } catch (Exception ex)
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                ex,
                getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return null;
        }

        return obj;
    }

    /** Returns the new dataset created. */
    public DataFormat getObject() { return newObject; }

    /** Returns the parent group of the new dataset. */
    public Group getParentGroup() {
        return (Group)groupList.get(parentChoice.getSelectedIndex());
    }

}
