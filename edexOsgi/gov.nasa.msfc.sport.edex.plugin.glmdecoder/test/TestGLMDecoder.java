import gov.nasa.msfc.sport.edex.glmdecoder.decoder.GLMDecoder;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import com.raytheon.uf.common.dataplugin.PluginDataObject;

public class TestGLMDecoder {

    public void processFile(String file) {
        try {
            byte[] array = readFileInAsByteArray(file);
            GLMDecoder decoder = new GLMDecoder();
            PluginDataObject[] objects = decoder.decode(array);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public byte[] readFileInAsByteArray(String file) throws IOException {
        Path path = Paths.get(file);

        return Files.readAllBytes(path);

    }

    public static void main(String[] args) {
        TestGLMDecoder testDecoder = new TestGLMDecoder();
        testDecoder
                .processFile("/data1/awips/sampledata/GLM/OR_GLM-L2-LCFA_G16_s20151831153096_e20151831153297_c20152020147422.nc");
        testDecoder
                .processFile("/data1/awips/sampledata/GLM/newsamples/glm/IXTR99_KNES_190001_18889.2015081900");
    }

}
