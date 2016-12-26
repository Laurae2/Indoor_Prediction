/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:06:57.185+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DRFModel named h2o_dt_03

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_dt_03 > h2o_dt_03.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_dt_03.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_dt_03", algorithm="drf")
public class h2o_dt_03 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }

  public boolean isSupervised() { return true; }
  public int nfeatures() { return 36; }
  public int nclasses() { return 5; }

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_dt_03.VALUES;
  // Number of output classes included in training data response column.
  public static final int NCLASSES = 5;

  // Column domains. The last array contains domain of response column.
  public static final String[][] DOMAINS = new String[][] {
    /* X1 */ null,
    /* X2 */ null,
    /* X3 */ null,
    /* X4 */ null,
    /* X5 */ null,
    /* X6 */ null,
    /* X7 */ null,
    /* X8 */ null,
    /* X9 */ null,
    /* X10 */ null,
    /* X11 */ null,
    /* X12 */ null,
    /* X13 */ null,
    /* X14 */ null,
    /* X15 */ null,
    /* X16 */ null,
    /* X17 */ null,
    /* X18 */ null,
    /* X19 */ null,
    /* X20 */ null,
    /* X21 */ null,
    /* X22 */ null,
    /* X23 */ null,
    /* X24 */ null,
    /* X25 */ null,
    /* X26 */ null,
    /* X27 */ null,
    /* X28 */ null,
    /* X29 */ null,
    /* X30 */ null,
    /* X31 */ null,
    /* X32 */ null,
    /* X33 */ null,
    /* X34 */ null,
    /* X35 */ null,
    /* X36 */ null,
    /* Label */ h2o_dt_03_ColInfo_36.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.27956989247311825,0.13978494623655913,0.15053763440860216,0.27956989247311825,0.15053763440860216};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = {0.27956989247311825,0.13978494623655913,0.15053763440860216,0.27956989247311825,0.15053763440860216};

  public h2o_dt_03() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-6618463638167551464L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    java.util.Arrays.fill(preds,0);
    h2o_dt_03_Forest_0.score0(data,preds);
    double sum = 0;
    for(int i=1; i<preds.length; i++) { sum += preds[i]; }
    if (sum>0) for(int i=1; i<preds.length; i++) { preds[i] /= sum; }
    preds[0] = hex.genmodel.GenModel.getPrediction(preds, PRIOR_CLASS_DISTRIB, data, 0.5);
    return preds;
  }
}
// The class representing training column names
class NamesHolder_h2o_dt_03 implements java.io.Serializable {
  public static final String[] VALUES = new String[36];
  static {
    NamesHolder_h2o_dt_03_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_dt_03_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "X1";
      sa[1] = "X2";
      sa[2] = "X3";
      sa[3] = "X4";
      sa[4] = "X5";
      sa[5] = "X6";
      sa[6] = "X7";
      sa[7] = "X8";
      sa[8] = "X9";
      sa[9] = "X10";
      sa[10] = "X11";
      sa[11] = "X12";
      sa[12] = "X13";
      sa[13] = "X14";
      sa[14] = "X15";
      sa[15] = "X16";
      sa[16] = "X17";
      sa[17] = "X18";
      sa[18] = "X19";
      sa[19] = "X20";
      sa[20] = "X21";
      sa[21] = "X22";
      sa[22] = "X23";
      sa[23] = "X24";
      sa[24] = "X25";
      sa[25] = "X26";
      sa[26] = "X27";
      sa[27] = "X28";
      sa[28] = "X29";
      sa[29] = "X30";
      sa[30] = "X31";
      sa[31] = "X32";
      sa[32] = "X33";
      sa[33] = "X34";
      sa[34] = "X35";
      sa[35] = "X36";
    }
  }
}
// The class representing column Label
class h2o_dt_03_ColInfo_36 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    h2o_dt_03_ColInfo_36_0.fill(VALUES);
  }
  static final class h2o_dt_03_ColInfo_36_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "4";
      sa[3] = "5";
      sa[4] = "6";
    }
  }
}

class h2o_dt_03_Forest_0 {
  public static void score0(double[] fdata, double[] preds) {
    preds[1] += h2o_dt_03_Tree_0_class_0.score0(fdata);
    preds[2] += h2o_dt_03_Tree_0_class_1.score0(fdata);
    preds[3] += h2o_dt_03_Tree_0_class_2.score0(fdata);
    preds[4] += h2o_dt_03_Tree_0_class_3.score0(fdata);
    preds[5] += h2o_dt_03_Tree_0_class_4.score0(fdata);
  }
}
class h2o_dt_03_Tree_0_class_0 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[3]) || data[3 /* X4 */] <0.059679415f ? 
         (data[0 /* X1 */] <-0.2331216f ? 
            1.0f : 
            0.0f) : 
         (Double.isNaN(data[15]) || data[15 /* X16 */] <0.10656356f ? 
             (Double.isNaN(data[21]) || data[21 /* X22 */] <0.19720636f ? 
                1.0f : 
                0.0f) : 
            0.0f));
    return pred;
  } // constant pool size = 18B, number of visited nodes = 4, static init size = 0B
}

class h2o_dt_03_Tree_0_class_1 {
  static final double score0(double[] data) {
    double pred =      (data[19 /* X20 */] <-0.39406702f ? 
         (data[33 /* X34 */] <-0.102165334f ? 
             (Double.isNaN(data[4]) || data[4 /* X5 */] <0.16801448f ? 
                0.0f : 
                1.0f) : 
            1.0f) : 
         (Double.isNaN(data[3]) || data[3 /* X4 */] <0.29629594f ? 
            0.0f : 
            1.0f));
    return pred;
  } // constant pool size = 18B, number of visited nodes = 4, static init size = 0B
}

class h2o_dt_03_Tree_0_class_2 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[19]) || data[19 /* X20 */] <0.07027615f ? 
         (Double.isNaN(data[24]) || data[24 /* X25 */] <0.29336238f ? 
            0.0f : 
             (data[1 /* X2 */] <-0.16346955f ? 
                0.0f : 
                1.0f)) : 
         (Double.isNaN(data[21]) || data[21 /* X22 */] <0.18740976f ? 
            1.0f : 
            0.0f));
    return pred;
  } // constant pool size = 18B, number of visited nodes = 4, static init size = 0B
}

class h2o_dt_03_Tree_0_class_3 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[34]) || data[34 /* X35 */] <0.38669977f ? 
         (Double.isNaN(data[35]) || data[35 /* X36 */] <0.119140625f ? 
            0.0f : 
             (data[19 /* X20 */] <-0.053236008f ? 
                1.0f : 
                0.0f)) : 
         (Double.isNaN(data[7]) || data[7 /* X8 */] <0.09271696f ? 
            1.0f : 
            0.0f));
    return pred;
  } // constant pool size = 18B, number of visited nodes = 4, static init size = 0B
}

class h2o_dt_03_Tree_0_class_4 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[27]) || data[27 /* X28 */] <0.28291926f ? 
         (data[12 /* X13 */] <0.090090126f ? 
            1.0f : 
            0.0f) : 
         (data[4 /* X5 */] <0.100837536f ? 
            0.0f : 
            1.0f));
    return pred;
  } // constant pool size = 14B, number of visited nodes = 3, static init size = 0B
}



