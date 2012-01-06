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
import java.io.File;
import java.util.List;
import java.util.Iterator;
import ncsa.hdf.object.*;

/**
 * FileConversionDialog shows a message dialog requesting user input for converting
 * files.
 * 
 * @author Peter X. Cao
 * @version 2.4 9/6/2007
 */
public class FileConversionDialog extends JDialog
implements ActionListener
{
	public static final long serialVersionUID = HObject.serialVersionUID;

    private String fileTypeFrom, fileTypeTo;

    private JTextField srcFileField, dstFileField;

    private boolean isConverted ;
    
    private boolean isConvertedFromImage;

    private String convertedFile;

    private String toFileExtension;

    private List fileList;

    private String currentDir;

    private final Toolkit toolkit;

     /**
      * Constructs a FileConversionDialog
      * @param owner The owner of the dialog.
      * @param typeFrom source file type
      * @param typeTo destinatin file type
      * @param dir current file directory
      * @param openFiles The list of current open files
      */
    public FileConversionDialog(
        Frame owner,
        String typeFrom,
        String typeTo,
        String dir,
        List openFiles)
    {
        super (owner, "Convert File...", true);

        fileTypeFrom = typeFrom;
        fileTypeTo = typeTo;
        isConverted = false;
        isConvertedFromImage = false;
        fileList = openFiles;
        toFileExtension = "";
        currentDir = dir;
        toolkit = Toolkit.getDefaultToolkit();

        String fromName = "Source";
        if (fileTypeTo.equals(FileFormat.FILE_TYPE_HDF5))
        {
            toFileExtension = ".h5";
            setTitle("Convert Image to HDF5 ...");
            fromName = "IMAGE";
            isConvertedFromImage = true;
        }
        else if (fileTypeTo.equals(FileFormat.FILE_TYPE_HDF4))
        {
            toFileExtension = ".hdf";
            setTitle("Convert Image to HDF4 ...");
            fromName = "IMAGE";
            isConvertedFromImage = true;
        }

        // layout the components
        JPanel contentPane = (JPanel)getContentPane();
        contentPane.setLayout(new BorderLayout(5,5));
        contentPane.setBorder(BorderFactory.createEmptyBorder(15,5,5,5));
        int w = 450 + (ViewProperties.getFontSize()-12)*15;
        int h = 120 + (ViewProperties.getFontSize()-12)*10;
        contentPane.setPreferredSize(new Dimension(w, h));

        // add the top panel for enter file name
        JPanel p = new JPanel();
        p.setLayout(new BorderLayout(5,5));

        JPanel p0 = new JPanel();
        p0.setLayout(new GridLayout(2, 1, 5, 5));
        p0.add(new JLabel(fromName +" File: "));
        p0.add(new JLabel("HDF File: "));
        p.add(p0, BorderLayout.WEST);

        p0 = new JPanel();
        p0.setLayout(new GridLayout(2, 1, 5, 5));
        p0.add(srcFileField = new JTextField());
        p0.add(dstFileField = new JTextField());
        p.add(p0, BorderLayout.CENTER);

        p0 = new JPanel();
        p0.setLayout(new GridLayout(2, 1, 5, 5));
        JButton jButton = new JButton("Browse...");
        jButton.setActionCommand("Browse source file");
        jButton.addActionListener(this);
        p0.add(jButton);
        jButton = new JButton("Browse...");
        jButton.setActionCommand("Browse target file");
        jButton.addActionListener(this);
        p0.add(jButton);
        p.add(p0, BorderLayout.EAST);

        contentPane.add(p, BorderLayout.CENTER);

        JButton okButton = new JButton("   Ok   ");
        okButton.setMnemonic(KeyEvent.VK_O);
        okButton.setActionCommand("Ok");
        okButton.addActionListener(this);

        JButton cancelButton = new JButton("Cancel");
        cancelButton.setMnemonic(KeyEvent.VK_C);
        cancelButton.setActionCommand("Cancel");
        cancelButton.addActionListener(this);


        p = new JPanel();
        p.add(okButton);
        p.add(cancelButton);

        contentPane.add(p, BorderLayout.SOUTH);

        Point l = owner.getLocation();
        l.x += 250;
        l.y += 80;
        setLocation(l);
        pack();
    }

    public void actionPerformed(ActionEvent e)
    {
        Object source = e.getSource();
        String cmd = e.getActionCommand();

        if (cmd.equals("Ok"))
        {
            isConverted = convert();

            if (isConverted) {
                dispose();
            }
        }
        else if (cmd.equals("Cancel"))
        {
            isConverted = false;
            convertedFile = null;
            dispose();
        }
        else if (cmd.equals("Browse source file"))
        {
            JFileChooser fchooser = new JFileChooser(currentDir);
            if (isConvertedFromImage)
                fchooser.setFileFilter(DefaultFileFilter.getImageFileFilter());

            int returnVal = fchooser.showOpenDialog(this);

            if(returnVal != JFileChooser.APPROVE_OPTION) {
                return;
            }

            File choosedFile = fchooser.getSelectedFile();
            if (choosedFile == null) {
                return;
            }

            String fname = choosedFile.getAbsolutePath();

            if (fname == null) {
                return;
            }

            currentDir = choosedFile.getParent();
            srcFileField.setText(fname);
            dstFileField.setText(fname+toFileExtension);
        }
        else if (cmd.equals("Browse target file"))
        {
            JFileChooser fchooser = new JFileChooser();
            int returnVal = fchooser.showOpenDialog(this);

            if(returnVal != JFileChooser.APPROVE_OPTION) {
                return;
            }

            File choosedFile = fchooser.getSelectedFile();
            if (choosedFile == null) {
                return;
            }

            String fname = choosedFile.getAbsolutePath();

            if (fname == null) {
                return;
            }

            dstFileField.setText(fname);
        }
    }

    /** convert file */
    private boolean convert()
    {
        boolean converted = false;
        String srcFile = srcFileField.getText();
        String dstFile = dstFileField.getText();

        if ((srcFile == null) || (dstFile == null))
        {
            return false;
        }

        srcFile = srcFile.trim();
        dstFile = dstFile.trim();
        if ((srcFile == null) || (srcFile.length()<=0) ||
            (dstFile == null) || (dstFile.length()<=0))
        {
            return false;
        }

        // verify the source file
        File f = new File(srcFile);
        if (!f.exists())
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Source file does not exist.",
                this.getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return false;
        }
        else if (f.isDirectory())
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Source file is a directory.",
                this.getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        // verify target file
        String srcPath = f.getParent();
        f = new File(dstFile);
        File pfile = f.getParentFile();
        if (pfile == null)
        {
            dstFile = srcPath + File.separator + dstFile;
            f = new File(dstFile);
        }
        else if ( !pfile.exists())
        {
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                "Destination file path does not exist at\n"+pfile.getPath(),
                this.getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        // check if the file is in use
        if (fileList != null)
        {
            FileFormat theFile = null;
            Iterator iterator = fileList.iterator();
            while(iterator.hasNext())
            {
                theFile = (FileFormat)iterator.next();
                if (theFile.getFilePath().equals(dstFile))
                {
                    toolkit.beep();
                    JOptionPane.showMessageDialog(this,
                        "The destination file is being used.",
                        getTitle(),
                        JOptionPane.ERROR_MESSAGE);
                    return false;
                }
            }
        }

        int newFileFlag = -1;
        if (f.exists())
        {
            newFileFlag = JOptionPane.showConfirmDialog(this,
                "Destination file exists. Do you want to replace it ?",
                this.getTitle(),
                JOptionPane.YES_NO_OPTION);
            if (newFileFlag == JOptionPane.NO_OPTION) {
                return false;
            }
        }

        try
        {
            Tools.convertImageToHDF(srcFile, dstFile, fileTypeFrom, fileTypeTo);
            convertedFile = dstFile;
            converted = true;
        } catch (Exception ex)
        {
            convertedFile = null;
            converted = false;
            toolkit.beep();
            JOptionPane.showMessageDialog(this,
                ex.getMessage(),
                this.getTitle(),
                JOptionPane.ERROR_MESSAGE);
            return false;
        }

        return converted;
    }

    public boolean isFileConverted()
    {
        return isConverted;
    }

    public String getConvertedFile()
    {
        return convertedFile;
    }
}


