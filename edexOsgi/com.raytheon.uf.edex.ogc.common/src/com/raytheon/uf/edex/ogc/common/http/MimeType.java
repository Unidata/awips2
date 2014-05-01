/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.http;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang.StringUtils;

/**
 * Data object representing a MIME type used in service requests
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */

public class MimeType {

    protected String type;
    
    protected String subtype;
    
    protected Map<String, String> parameters;
    
    private static final String TOKEN_CLASS = "[^ \\t\\(\\)\\\\\\/@,;:<>\\[\\]\\?=\"]";

    private static final Pattern TYPE_PATTERN = Pattern.compile("^\\s*("
            + TOKEN_CLASS + "+)/?(" + TOKEN_CLASS + "+)?.*$");

    private static final Pattern PARAM_PATTERN = Pattern.compile(";\\s*("
            + TOKEN_CLASS + "+)=(" + TOKEN_CLASS + "+|\"\\S+\")");
    
    public MimeType(String mime){
        Matcher m = TYPE_PATTERN.matcher(mime);
        if (m.matches()) {
            this.type = m.group(1).toLowerCase();
            int typeEnd;
            this.subtype = m.group(2);
            if (this.subtype != null) {
                typeEnd = m.end(2);
                this.subtype = this.subtype.toLowerCase();
            } else {
                typeEnd = m.end(1);
            }
            String params = mime.substring(typeEnd);
            this.parameters = getParameters(params);
        } else {
            throw new IllegalArgumentException("Invalid mime type string: "
                    + mime);
        }
    }

    private static Map<String, String> getParameters(String paramStr) {
        Matcher m = PARAM_PATTERN.matcher(paramStr);
        Map<String, String> rval = new LinkedHashMap<String, String>();
        while (m.find()) {
            String param = m.group(1).toLowerCase();
            String value = StringUtils.strip(m.group(2), "\"");
            rval.put(param, value);
        }
        return Collections.unmodifiableMap(rval);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result
                + ((parameters == null) ? 0 : parameters.hashCode());
        result = prime * result + ((subtype == null) ? 0 : subtype.hashCode());
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        MimeType other = (MimeType) obj;
        if (parameters == null) {
            if (other.parameters != null)
                return false;
        } else {
            if (other.parameters == null) {
                return false;
            }
            if (parameters.size() != other.parameters.size()) {
                return false;
            }
            for (String s : parameters.keySet()) {
                String val = parameters.get(s);
                if (!val.equalsIgnoreCase(other.parameters.get(s))) {
                    return false;
                }
            }
        }
        if (subtype == null) {
            if (other.subtype != null)
                return false;
        } else if (!subtype.equalsIgnoreCase(other.subtype))
            return false;
        if (type == null) {
            if (other.type != null)
                return false;
        } else if (!type.equalsIgnoreCase(other.type))
            return false;
        return true;
    }

    public boolean equalsIgnoreParams(MimeType other) {
        if (other == null) {
            return false;
        }
        if (subtype == null) {
            if (other.subtype != null)
                return false;
        } else if (!subtype.equalsIgnoreCase(other.subtype))
            return false;
        if (type == null) {
            if (other.type != null)
                return false;
        } else if (!type.equalsIgnoreCase(other.type))
            return false;
        return true;
    }

    /**
     * @param paramName
     * @return null if parameter is not found
     */
    public String getParam(String paramName) {
        return parameters.get(paramName.toLowerCase());
    }

    public int getNumParams() {
        return parameters.size();
    }

    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder(toStringWithoutParams());
        for ( String key : this.parameters.keySet()){
            sb.append("; ").append(key);
            sb.append("=");
            String value = this.parameters.get(key);
            if (value.matches("^" + TOKEN_CLASS + "+$")) {
                sb.append(value);
            } else {
                sb.append("\"").append(value).append("\"");
            }
        }
        return sb.toString();
    }

    public String toStringWithoutParams() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.type).append("/");
        sb.append(this.subtype);
        return sb.toString();
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }


    /**
     * @return the subtype
     */
    public String getSubtype() {
        return subtype;
    }

}
