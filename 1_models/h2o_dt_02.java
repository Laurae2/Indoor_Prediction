/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T13:46:46.168+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DRFModel named h2o_dt_02

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_dt_02 > h2o_dt_02.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_dt_02.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_dt_02", algorithm="drf")
public class h2o_dt_02 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }

  public boolean isSupervised() { return true; }
  public int nfeatures() { return 36; }
  public int nclasses() { return 5; }

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_dt_02.VALUES;
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
    /* Label */ h2o_dt_02_ColInfo_36.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.25,0.25,0.125,0.25,0.125};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = {0.25,0.25,0.125,0.25,0.125};

  public h2o_dt_02() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(7511430394244784080L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    java.util.Arrays.fill(preds,0);
    h2o_dt_02_Forest_0.score0(data,preds);
    double sum = 0;
    for(int i=1; i<preds.length; i++) { sum += preds[i]; }
    if (sum>0) for(int i=1; i<preds.length; i++) { preds[i] /= sum; }
    preds[0] = hex.genmodel.GenModel.getPrediction(preds, PRIOR_CLASS_DISTRIB, data, 0.5);
    return preds;
  }
}
// The class representing training column names
class NamesHolder_h2o_dt_02 implements java.io.Serializable {
  public static final String[] VALUES = new String[36];
  static {
    NamesHolder_h2o_dt_02_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_dt_02_0 implements java.io.Serializable {
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
class h2o_dt_02_ColInfo_36 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    h2o_dt_02_ColInfo_36_0.fill(VALUES);
  }
  static final class h2o_dt_02_ColInfo_36_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "4";
      sa[3] = "5";
      sa[4] = "6";
    }
  }
}

class h2o_dt_02_Forest_0 {
  public static void score0(double[] fdata, double[] preds) {
    preds[1] += h2o_dt_02_Tree_0_class_0.score0(fdata);
    preds[2] += h2o_dt_02_Tree_0_class_1.score0(fdata);
    preds[3] += h2o_dt_02_Tree_0_class_2.score0(fdata);
    preds[4] += h2o_dt_02_Tree_0_class_3.score0(fdata);
    preds[5] += h2o_dt_02_Tree_0_class_4.score0(fdata);
  }
}
class h2o_dt_02_Tree_0_class_0 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[34]) || data[34 /* X35 */] <-0.023438476f ? 
         (data[15 /* X16 */] <0.105073646f ? 
             (Double.isNaN(data[11]) || data[11 /* X12 */] <0.0038190503f ? 
                1.0f : 
                0.0f) : 
            0.0f) : 
         (data[1 /* X2 */] <-0.17085432f ? 
            1.0f : 
             (data[15 /* X16 */] <0.08445112f ? 
                1.0f : 
                 (data[3 /* X4 */] <-0.7489356f ? 
                     (data[3 /* X4 */] <-0.9097731f ? 
                        0.0f : 
                        1.0f) : 
                    0.0f))));
    return pred;
  } // constant pool size = 30B, number of visited nodes = 7, static init size = 0B
}

class h2o_dt_02_Tree_0_class_1 {
  static final double score0(double[] data) {
    double pred =      (data[3 /* X4 */] <-0.49490413f ? 
         (Double.isNaN(data[7]) || data[7 /* X8 */] <0.1046651f ? 
            1.0f : 
             (Double.isNaN(data[16]) || data[16 /* X17 */] <0.21538177f ? 
                0.0f : 
                1.0f)) : 
        0.0f);
    return pred;
  } // constant pool size = 14B, number of visited nodes = 3, static init size = 0B
}

class h2o_dt_02_Tree_0_class_2 {
  static final double score0(double[] data) {
    double pred =      (data[10 /* X11 */] <-0.5653937f ? 
         (Double.isNaN(data[2]) || data[2 /* X3 */] <0.44658968f ? 
            1.0f : 
            0.0f) : 
         (data[12 /* X13 */] <0.1337939f ? 
             (Double.isNaN(data[1]) || data[1 /* X2 */] <0.008503408f ? 
                1.0f : 
                0.0f) : 
             (Double.isNaN(data[5]) || data[5 /* X6 */] <0.46869475f ? 
                 (Double.isNaN(data[5]) || data[5 /* X6 */] <0.38709694f ? 
                     (Double.isNaN(data[12]) || data[12 /* X13 */] <0.31485277f ? 
                        0.0f : 
                         (data[12 /* X13 */] <0.32165313f ? 
                            1.0f : 
                            0.0f)) : 
                     (Double.isNaN(data[1]) || data[1 /* X2 */] <-0.037694793f ? 
                        0.0f : 
                        1.0f)) : 
                1.0f)));
    return pred;
  } // constant pool size = 38B, number of visited nodes = 9, static init size = 0B
}

class h2o_dt_02_Tree_0_class_3 {
  static final double score0(double[] data) {
    double pred =      (data[3 /* X4 */] <-0.1451151f ? 
        0.0f : 
         (Double.isNaN(data[11]) || data[11 /* X12 */] <0.9916381f ? 
             (Double.isNaN(data[27]) || data[27 /* X28 */] <-0.19146678f ? 
                 (data[25 /* X26 */] <-0.3890302f ? 
                    0.0f : 
                     (data[34 /* X35 */] <-0.9241075f ? 
                        0.0f : 
                         (Double.isNaN(data[28]) || data[28 /* X29 */] <0.3047288f ? 
                             (Double.isNaN(data[30]) || data[30 /* X31 */] <0.37955838f ? 
                                 (Double.isNaN(data[18]) || data[18 /* X19 */] <0.3849178f ? 
                                    1.0f : 
                                     (data[0 /* X1 */] <0.082902215f ? 
                                        0.0f : 
                                        1.0f)) : 
                                0.0f) : 
                             (data[0 /* X1 */] <-0.013686874f ? 
                                1.0f : 
                                0.0f)))) : 
                 (Double.isNaN(data[9]) || data[9 /* X10 */] <0.041091688f ? 
                    0.0f : 
                    1.0f)) : 
            0.0f));
    return pred;
  } // constant pool size = 46B, number of visited nodes = 11, static init size = 0B
}

class h2o_dt_02_Tree_0_class_4 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[11]) || data[11 /* X12 */] <1.0273732f ? 
         (data[34 /* X35 */] <-0.9274554f ? 
             (Double.isNaN(data[0]) || data[0 /* X1 */] <0.07859138f ? 
                1.0f : 
                0.0f) : 
             (Double.isNaN(data[33]) || data[33 /* X34 */] <0.50046873f ? 
                0.0f : 
                 (data[5 /* X6 */] <0.22990827f ? 
                     (data[1 /* X2 */] <-0.14356215f ? 
                        0.0f : 
                        1.0f) : 
                    0.0f))) : 
        1.0f);
    return pred;
  } // constant pool size = 26B, number of visited nodes = 6, static init size = 0B
}



