package gov.noaa.nws.ncep.viz.rsc.solarimage.util;

/**
 * <pre>
 * Provide image functions. Format:
 * imageFunction=functionName(parameter…) 
 * 
 * Date         Ticket#    Engineer        Description
 * ------------ ---------- -----------     --------------------------
 * 12/17/2013   1046       qzhou           initial
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
public class ImageFunctionParser {

    public static String[] parse(String imageFunction) {
        String[] parse = null;

        String regex = "\\(|,|\\)";

        if (imageFunction != "")
            parse = imageFunction.split(regex);

        return parse;
    }

    public static String getFuncName(String imageFunction) {
        String funcName = null;

        String[] parse = parse(imageFunction);

        if (parse != null && parse.length >= 1)
            funcName = parse[0];

        return funcName;
    }

    public static int getFuncParamNum(String imageFunction) {
        String[] parse = parse(imageFunction);
        if (parse.length > 1)
            return parse.length - 1;
        else
            return 0;
    }

    public static String getFuncParam(String imageFunction, int paramOrder) {
        String param = null;

        String[] parse = parse(imageFunction);

        if (parse != null && parse.length > paramOrder)
            param = parse[paramOrder];

        return param;
    }

}
