/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.subscription.data;

import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.quartz.CronExpression;

import com.raytheon.uf.common.dataplugin.persist.IPersistableDataObject;
import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Class representing the subscriptions table in the subscription database
 * schema.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 18Nov2008    1709       MW Fegan    Initial creation
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */
@Entity
@Table(name = "subscriptions", schema = "subscription", uniqueConstraints = @UniqueConstraint(columnNames = {
        "type", "trigger", "runner", "script", "filepath", "arguments" }))
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class SubscriptionRecord extends PersistableDataObject implements
        IPersistableDataObject {

    private static final long serialVersionUID = 1L;

    @XmlAttribute
    @DynamicSerializeElement
    @Id
    @GeneratedValue
    private long id;

    @XmlAttribute
    @DynamicSerializeElement
    @Column
    private boolean active;

    @XmlAttribute
    @DynamicSerializeElement
    @Column(length = 10)
    private String type = "";

    @XmlAttribute
    @DynamicSerializeElement
    @Column(length = 512)
    private String trigger = "";

    @XmlAttribute
    @DynamicSerializeElement
    @Column(length = 10)
    private String runner = "";

    @XmlAttribute
    @DynamicSerializeElement
    @Column(length = 4048)
    private String script = "";

    @XmlAttribute
    @DynamicSerializeElement
    @Column(length = 512)
    private String filepath = "";

    @XmlAttribute
    @DynamicSerializeElement
    @Column(length = 512)
    private String arguments = "";

    /* the replacement values */
    @DynamicSerializeElement
    @XmlElement
    @OneToMany(cascade = CascadeType.ALL, mappedBy = "subscription", fetch = FetchType.EAGER)
    private Set<ReplacementRecord> replacements = new HashSet<ReplacementRecord>();

    /**
     * 
     */
    public SubscriptionRecord() {
        super();
    }

    /**
     * @return the id
     */
    public long getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(long id) {
        this.id = id;
    }

    /**
     * @return the active
     */
    public boolean isActive() {
        return active;
    }

    /**
     * @param active
     *            the active to set
     */
    public void setActive(boolean active) {
        this.active = active;
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        if (type == null) {
            this.type = "";
        } else {
            this.type = type;
        }
    }

    /**
     * @return the trigger
     */
    public String getTrigger() {
        return trigger;
    }

    /**
     * @param trigger
     *            the trigger to set
     */
    public void setTrigger(String trigger) {
        if (trigger == null) {
            this.trigger = "";
        } else {
            this.trigger = trigger;
        }
    }

    /**
     * @return the runner
     */
    public String getRunner() {
        return runner;
    }

    /**
     * @param runner
     *            the runner to set
     */
    public void setRunner(String runner) {
        if (runner == null) {
            this.runner = "";
        } else {
            this.runner = runner;
        }
    }

    /**
     * @return the script
     */
    public String getScript() {
        return script;
    }

    /**
     * @param script
     *            the script to set
     */
    public void setScript(String script) {
        if (script == null) {
            this.script = "";
        } else {
            this.script = script;
        }
    }

    /**
     * @return the filepath
     */
    public String getFilepath() {
        return filepath;
    }

    /**
     * @param filepath
     *            the filepath to set
     */
    public void setFilepath(String filepath) {
        if (filepath == null) {
            this.filepath = "";
        } else {
            this.filepath = filepath;
        }
    }

    /**
     * @return the arguments
     */
    public String getArguments() {
        return arguments;
    }

    /**
     * @param arguments
     *            the arguments to set
     */
    public void setArguments(String arguments) {
        if (arguments == null) {
            this.arguments = "";
        } else {
            this.arguments = arguments;
        }
    }

    /**
     * Determines if the specified trigger value matches the trigger value of
     * this SubscriptionRecord.
     * 
     * @param trigger
     *            the trigger value to compare
     * 
     * @return true if the trigger matches, false otherwise
     */
    public boolean matchesTrigger(String trigger) {
        boolean retVal = false;
        if ("timer".equalsIgnoreCase(this.type)) {
            try {
                long date = Long.parseLong(trigger);
                CronExpression cexp = new CronExpression(this.trigger);
                retVal = cexp.isSatisfiedBy(new Date(date));
            } catch (Exception e) {
                retVal = false;
            }
        } else if ("ldad".equalsIgnoreCase(this.type)) {
            /*
             * Legacy code has these patterns: TextString patterns[] = {
             * productId, productId.left(6) + "XXX", "CCC" + productId.mid(3,3)
             * + "XXX", productId.left(3) + "NNNXXX", productId.left(3) + "NNN"
             * + productId.right(productId.length() - 6) };
             */
            // System.out.println("matching trigger=" + trigger +
            // " -- this.trigger="+ this.trigger +
            // " -- trigger.substr(0,6)+\"XXX\"="+trigger.substring(0,6)+"XXX" +
            // " -- \"CCC\"+trigger.substring(3,6)+\"XXX\"="+"CCC"+trigger.substring(3,
            // 6)+"XXX" +
            // " -- trigger.substring(0,3)+\"NNNXXX\"=" + trigger.substring(0,
            // 3)+"NNNXXX" +
            // " -- trigger.substring(0, 3)+\"NNN\"+trigger.substring(6)="+trigger.substring(0,
            // 3)+"NNN"+trigger.substring(6));
            retVal = (trigger.equalsIgnoreCase(this.trigger))
                    || ((trigger.substring(0, 6) + "XXX")
                            .equalsIgnoreCase(this.trigger))
                    || (("CCC" + trigger.substring(3, 6) + "XXX")
                            .equalsIgnoreCase(this.trigger))
                    || ((trigger.substring(0, 3) + "NNNXXX")
                            .equalsIgnoreCase(this.trigger))
                    || ((trigger.substring(0, 3) + "NNN" + trigger.substring(6))
                            .equalsIgnoreCase(this.trigger) || (this.trigger
                            .startsWith(trigger)));
        } else {
            String pattern = this.trigger.replaceAll("\\*", ".+");
            retVal = trigger.matches(pattern);
        }
        return retVal;
    }

    /**
     * 
     * @param name
     * @param value
     */
    public void setAttribute(String name, String value) {
        switch (Settable.getSettable(name)) {
        case TRIGGER:
            this.trigger = value;
            break;
        case RUNNER:
            this.runner = value;
            break;
        case TYPE:
            this.type = value;
            break;
        case COMMAND:
            this.arguments = value;
            break;
        case FILE:
            this.filepath = value;
            break;
        case SCRIPT:
            this.script = value;
            break;
        case ACTIVE:
            this.active = Boolean.parseBoolean(value);
            break;
        case ARGUMENTS:
            this.arguments = value;
            break;
        case SUBSTITUTION:
            String[] parts = value.split(":");
            ReplacementRecord rec = new ReplacementRecord(this, parts[0],
                    parts[1]);
            this.replacements.add(rec);
            break;
        case DEFAULT:
            // nothing to do here...
        }
    }

    /**
     * @return the replacements
     */
    public Set<ReplacementRecord> getReplacements() {
        return replacements;
    }

    /**
     * @param replacements
     *            the replacements to set
     */
    public void setReplacements(Set<ReplacementRecord> replacements) {
        this.replacements = replacements;
    }

    /**
     * 
     * @param replacement
     */
    public void addReplacement(ReplacementRecord replacement) {
        this.replacements.add(replacement);
        replacement.setSubscription(this);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getClass().getSimpleName()).append("[");
        sb.append("ID=").append(this.id).append(",");
        sb.append("active='").append(this.active).append("',");
        sb.append("type='").append(this.type).append("',");
        sb.append("trigger='").append(this.trigger).append("',");
        sb.append("runner='").append(this.runner).append("'");
        if (this.script != null) {
            sb.append(",script='").append(this.script).append("'");
        }
        if (this.filepath != null) {
            sb.append(",filepath='").append(this.filepath).append("'");
        }
        if (this.arguments != null) {
            sb.append(",arguments=").append(this.arguments);
        }
        if (this.replacements.size() > 0) {
            sb.append(",replacements=[").append(this.replacements);
        }
        sb.append("]");
        return sb.toString();
    }

    /**
     * 
     * @author mfegan
     * @version 1.0
     */
    private enum Settable {
        DEFAULT, RUNNER, TRIGGER, TYPE, COMMAND, FILE, SCRIPT, SUBSTITUTION, ACTIVE, ARGUMENTS;
        private static final Map<String, Settable> mappings = new HashMap<String, Settable>() {
            private static final long serialVersionUID = 1L;
            {
                put("runner", RUNNER);
                put("type", TYPE);
                put("trigger", TRIGGER);
                put("command", COMMAND);
                put("file", FILE);
                put("filepath", FILE);
                put("script", SCRIPT);
                put("substitution", SUBSTITUTION);
                put("active", ACTIVE);
                put("arguments", ARGUMENTS);
            }
        };

        public static final Settable getSettable(String key) {
            return mappings.containsKey(key) ? mappings.get(key) : DEFAULT;
        }
    }
}
