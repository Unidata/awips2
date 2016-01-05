package com.raytheon.viz.hydrobase.addEditFloodTS;

import java.util.Calendar;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * This composite customized with a insert label, an observation(a value text, 2
 * time widgets), and a delete label.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ----------   ----------  ----------- --------------------------
 * 11/05/2015   DCS15095    wkwock      Initial creation
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */
public class TSComposite extends Composite {
    private Text valueTxt;

    private DateTime date;

    private DateTime time;

    private Label deleteLbl;

    private Label insertLbl;

    private ITSCompositeAction actionCallback  = null;

    /**
     * Constructor for composite with observe value and time
     * 
     * @param parent
     * @param obsValue
     * @param cal
     * @param owner
     */
    public TSComposite(Composite parent, double obsValue, Calendar cal, ITSCompositeAction owner) {
        super(parent, SWT.NONE);
        this.actionCallback = owner;

        createAnObsComp();
        setValue(obsValue);
        setDateTime(cal);
    }

    /**
     * set the value to the value text box
     * 
     * @param obsValue
     */
    public void setValue(double obsValue) {
        valueTxt.setText(Double.toString(obsValue));
    }

    /**
     * Get the value from the value text box
     * 
     * @return double value
     * @throws NumberFormatException
     */
    public double getValue() throws NumberFormatException {
        return Double.parseDouble(valueTxt.getText());
    }

    /**
     * set date time to both date time box
     * 
     * @param cal
     */
    public void setDateTime(Calendar cal) {
        date.setDate(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH),
                cal.get(Calendar.DATE));
        date.setTime(cal.get(Calendar.HOUR_OF_DAY), cal.get(Calendar.MINUTE), 0);

        time.setDate(cal.get(Calendar.YEAR), cal.get(Calendar.MONTH),
                cal.get(Calendar.DATE));
        time.setTime(cal.get(Calendar.HOUR_OF_DAY), cal.get(Calendar.MINUTE), 0);
    }

    /**
     * Get date time
     * @return date time
     */
    public Calendar getDateTime() {
        Calendar cal = Calendar.getInstance();
        cal.set(date.getYear(), date.getMonth(), date.getDay(),
                time.getHours(), time.getMinutes(), 0);

        return cal;
    }

    /**
     * create an observe composite
     */
    private void createAnObsComp() {
        FormLayout cfl = new FormLayout();
        cfl.marginWidth = 5;
        cfl.marginHeight = 2;
        this.setLayout(cfl);

        valueTxt = new Text(this, SWT.BORDER);
        FormData vfd = new FormData();
        vfd.width = 50;
        vfd.height = 18;
        valueTxt.setLayoutData(vfd);
        valueTxt.setText("0.0");

        date = new DateTime(this, SWT.DATE | SWT.BORDER);
        FormData fd = new FormData();
        fd.left = new FormAttachment(valueTxt, 10);
        fd.width = 100;
        date.setLayoutData(fd);

        time = new DateTime(this, SWT.TIME | SWT.BORDER | SWT.SHORT);
        FormData timeFd = new FormData();
        timeFd.left = new FormAttachment(date, 10);
        timeFd.width = 100;
        time.setLayoutData(timeFd);

        Cursor handCursor = this.getDisplay().getSystemCursor(SWT.CURSOR_HAND);
        deleteLbl = new Label(this, SWT.NONE);
        deleteLbl
                .setForeground(this.getDisplay().getSystemColor(SWT.COLOR_RED));
        deleteLbl.setText("x");
        deleteLbl.setCursor(handCursor);
        FormData dfd = new FormData();
        dfd.left = new FormAttachment(time, 10);
        dfd.top = new FormAttachment(this, 5);
        deleteLbl.setLayoutData(dfd);

        insertLbl = new Label(this, SWT.NONE);
        insertLbl.setForeground(this.getDisplay().getSystemColor(
                SWT.COLOR_GREEN));
        insertLbl.setText("-->");
        insertLbl.setCursor(handCursor);
        FormData insertFd = new FormData();
        insertFd.top = new FormAttachment(valueTxt, 4);
        insertLbl.setLayoutData(insertFd);

        final TSComposite tsComp=this;
        deleteLbl.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent event) {
                actionCallback.removeTSComp(tsComp);
            }
        });

        insertLbl.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseUp(MouseEvent event) {
                actionCallback.addTSComp(tsComp);
            }
        });
    }
}
