/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:41:32.301+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for GLMModel named h2o_glm_04

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_glm_04 > h2o_glm_04.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_glm_04.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_glm_04", algorithm="glm")
public class h2o_glm_04 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }

  public boolean isSupervised() { return true; }
  public int nfeatures() { return 13; }
  public int nclasses() { return 6; }

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_glm_04.VALUES;
  // Number of output classes included in training data response column.
  public static final int NCLASSES = 6;

  // Column domains. The last array contains domain of response column.
  public static final String[][] DOMAINS = new String[][] {
    /* X5 */ null,
    /* X12 */ null,
    /* X15 */ null,
    /* X17 */ null,
    /* X18 */ null,
    /* X19 */ null,
    /* X20 */ null,
    /* X21 */ null,
    /* X23 */ null,
    /* X27 */ null,
    /* X28 */ null,
    /* X34 */ null,
    /* X35 */ null,
    /* Label */ h2o_glm_04_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.24528301886792453,0.12264150943396226,0.12264150943396226,0.1320754716981132,0.24528301886792453,0.1320754716981132};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_glm_04() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-3754289772554286960L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    final double [] b = BETA.VALUES;
    for(int i = 0; i < 0; ++i) if(Double.isNaN(data[i])) data[i] = CAT_MODES.VALUES[i];
    for(int i = 0; i < 13; ++i) if(Double.isNaN(data[i + 0])) data[i+0] = NUM_MEANS.VALUES[i];
    preds[0] = 0;
    for(int c = 0; c < 6; ++c){
      preds[c+1] = 0;
      for(int i = 0; i < 13; ++i)
        preds[c+1] += b[0+i + c*14]*data[i];
      preds[c+1] += b[13 + c*14]; // reduce intercept
    }
    double max_row = 0;
    for(int c = 1; c < preds.length; ++c) if(preds[c] > max_row) max_row = preds[c];
    double sum_exp = 0;
    for(int c = 1; c < preds.length; ++c) { sum_exp += (preds[c] = Math.exp(preds[c]-max_row));}
    sum_exp = 1/sum_exp;
    double max_p = 0;
    for(int c = 1; c < preds.length; ++c) if((preds[c] *= sum_exp) > max_p){ max_p = preds[c]; preds[0] = c-1;};
    return preds;
  }
    public static class BETA implements java.io.Serializable {
      public static final double[] VALUES = new double[84];
      static {
        BETA_0.fill(VALUES);
      }
      static final class BETA_0 implements java.io.Serializable {
        static final void fill(double[] sa) {
          sa[0] = 0.0;
          sa[1] = 0.0;
          sa[2] = 0.0;
          sa[3] = 0.0;
          sa[4] = -0.5546232780133566;
          sa[5] = 0.0;
          sa[6] = 0.0;
          sa[7] = 0.0;
          sa[8] = 0.0;
          sa[9] = 0.0;
          sa[10] = -0.1016019213883923;
          sa[11] = 0.47680656841031344;
          sa[12] = -1.0498290322187218;
          sa[13] = -1.2955875668712311;
          sa[14] = 0.0;
          sa[15] = 0.0;
          sa[16] = 0.0;
          sa[17] = 0.0;
          sa[18] = 0.0;
          sa[19] = 0.0;
          sa[20] = -0.9183414838989188;
          sa[21] = 0.0;
          sa[22] = 0.0;
          sa[23] = 0.0;
          sa[24] = 0.0;
          sa[25] = 0.7227357931203247;
          sa[26] = -0.3229702919501436;
          sa[27] = -1.7472386265033715;
          sa[28] = 0.0;
          sa[29] = 1.4152967015654163;
          sa[30] = 0.0;
          sa[31] = 0.0;
          sa[32] = 0.0;
          sa[33] = 0.0;
          sa[34] = 0.0;
          sa[35] = 0.0;
          sa[36] = 0.0;
          sa[37] = 0.0;
          sa[38] = 0.0;
          sa[39] = 1.0288228344065786;
          sa[40] = -0.9411447403418981;
          sa[41] = -1.6918952192403538;
          sa[42] = 0.0;
          sa[43] = 0.0;
          sa[44] = 0.0;
          sa[45] = 0.4546217765454378;
          sa[46] = 0.0;
          sa[47] = 0.0;
          sa[48] = 0.6365730855629982;
          sa[49] = 0.0;
          sa[50] = 0.0;
          sa[51] = 0.0;
          sa[52] = 0.0;
          sa[53] = -0.9491008291496185;
          sa[54] = 0.0;
          sa[55] = -1.6595044647785924;
          sa[56] = 0.0;
          sa[57] = 0.0;
          sa[58] = 0.0;
          sa[59] = 0.0;
          sa[60] = 0.0;
          sa[61] = 0.42045598019710567;
          sa[62] = 0.0;
          sa[63] = 0.0;
          sa[64] = 0.0;
          sa[65] = 0.0;
          sa[66] = 0.0;
          sa[67] = -0.3787547707194873;
          sa[68] = 1.3888672593542526;
          sa[69] = -1.2818309731647644;
          sa[70] = 0.0;
          sa[71] = -0.49065416766779263;
          sa[72] = 0.0;
          sa[73] = 0.0;
          sa[74] = 0.0;
          sa[75] = 0.0;
          sa[76] = 0.0;
          sa[77] = 0.0;
          sa[78] = 0.0;
          sa[79] = 0.0;
          sa[80] = 1.8175500421966042;
          sa[81] = 0.0;
          sa[82] = 0.359144358543715;
          sa[83] = -1.929316394968231;
        }
      }
}
// Imputed numeric values
    static class NUM_MEANS implements java.io.Serializable {
      public static final double[] VALUES = new double[13];
      static {
        NUM_MEANS_0.fill(VALUES);
      }
      static final class NUM_MEANS_0 implements java.io.Serializable {
        static final void fill(double[] sa) {
          sa[0] = 0.13289582332404562;
          sa[1] = -0.21432588409085435;
          sa[2] = 0.16740760283984527;
          sa[3] = -0.2406530780711871;
          sa[4] = -0.20879022991702365;
          sa[5] = 0.25691536440336205;
          sa[6] = -0.1726023939589246;
          sa[7] = 0.16222407386792506;
          sa[8] = 0.15952512463493737;
          sa[9] = 0.1713188059845201;
          sa[10] = 0.053149357129114345;
          sa[11] = -0.17368165094339627;
          sa[12] = -0.16938239622641513;
        }
      }
}
// Imputed categorical values.
    static class CAT_MODES implements java.io.Serializable {
      public static final int[] VALUES = new int[0];
      static {
      }
}
    // Categorical Offsets
    public static final int[] CATOFFS = {0};
}
// The class representing training column names
class NamesHolder_h2o_glm_04 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_glm_04_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_glm_04_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "X5";
      sa[1] = "X12";
      sa[2] = "X15";
      sa[3] = "X17";
      sa[4] = "X18";
      sa[5] = "X19";
      sa[6] = "X20";
      sa[7] = "X21";
      sa[8] = "X23";
      sa[9] = "X27";
      sa[10] = "X28";
      sa[11] = "X34";
      sa[12] = "X35";
    }
  }
}
// The class representing column Label
class h2o_glm_04_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_glm_04_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_glm_04_ColInfo_13_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "3";
      sa[3] = "4";
      sa[4] = "5";
      sa[5] = "6";
    }
  }
}


