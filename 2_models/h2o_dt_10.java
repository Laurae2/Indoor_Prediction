/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T13:52:06.044+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DRFModel named h2o_dt_10

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_dt_10 > h2o_dt_10.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_dt_10.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_dt_10", algorithm="drf")
public class h2o_dt_10 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }

  public boolean isSupervised() { return true; }
  public int nfeatures() { return 36; }
  public int nclasses() { return 6; }

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_dt_10.VALUES;
  // Number of output classes included in training data response column.
  public static final int NCLASSES = 6;

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
    /* Label */ h2o_dt_10_ColInfo_36.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.24761904761904763,0.18571428571428572,0.06190476190476191,0.12857142857142856,0.24761904761904763,0.12857142857142856};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = {0.24761904761904763,0.18571428571428572,0.06190476190476191,0.12857142857142856,0.24761904761904763,0.12857142857142856};

  public h2o_dt_10() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-6625128017903342452L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    java.util.Arrays.fill(preds,0);
    h2o_dt_10_Forest_0.score0(data,preds);
    double sum = 0;
    for(int i=1; i<preds.length; i++) { sum += preds[i]; }
    if (sum>0) for(int i=1; i<preds.length; i++) { preds[i] /= sum; }
    preds[0] = hex.genmodel.GenModel.getPrediction(preds, PRIOR_CLASS_DISTRIB, data, 0.5);
    return preds;
  }
}
// The class representing training column names
class NamesHolder_h2o_dt_10 implements java.io.Serializable {
  public static final String[] VALUES = new String[36];
  static {
    NamesHolder_h2o_dt_10_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_dt_10_0 implements java.io.Serializable {
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
class h2o_dt_10_ColInfo_36 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_dt_10_ColInfo_36_0.fill(VALUES);
  }
  static final class h2o_dt_10_ColInfo_36_0 implements java.io.Serializable {
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

class h2o_dt_10_Forest_0 {
  public static void score0(double[] fdata, double[] preds) {
    preds[1] += h2o_dt_10_Tree_0_class_0.score0(fdata);
    preds[2] += h2o_dt_10_Tree_0_class_1.score0(fdata);
    preds[3] += h2o_dt_10_Tree_0_class_2.score0(fdata);
    preds[4] += h2o_dt_10_Tree_0_class_3.score0(fdata);
    preds[5] += h2o_dt_10_Tree_0_class_4.score0(fdata);
    preds[6] += h2o_dt_10_Tree_0_class_5.score0(fdata);
  }
}
class h2o_dt_10_Tree_0_class_0 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[35]) || data[35 /* X36 */] <-0.111666374f ? 
         (data[16 /* X17 */] <-0.41286424f ? 
             (Double.isNaN(data[3]) || data[3 /* X4 */] <0.35579205f ? 
                 (data[3 /* X4 */] <-0.2241999f ? 
                    0.0f : 
                     (Double.isNaN(data[26]) || data[26 /* X27 */] <0.53109854f ? 
                        1.0f : 
                        0.0f)) : 
                0.0f) : 
             (data[15 /* X16 */] <0.10505192f ? 
                 (data[3 /* X4 */] <4.5595873E-5f ? 
                     (data[1 /* X2 */] <-0.6076541f ? 
                        1.0f : 
                        0.0f) : 
                     (Double.isNaN(data[1]) || data[1 /* X2 */] <0.22024117f ? 
                        1.0f : 
                        0.0f)) : 
                 (Double.isNaN(data[7]) || data[7 /* X8 */] <0.18493882f ? 
                     (Double.isNaN(data[20]) || data[20 /* X21 */] <0.66733694f ? 
                         (data[17 /* X18 */] <-0.6849147f ? 
                            1.0f : 
                             (Double.isNaN(data[24]) || data[24 /* X25 */] <0.7603048f ? 
                                0.0f : 
                                 (data[0 /* X1 */] <0.10483447f ? 
                                    0.0f : 
                                    1.0f))) : 
                         (Double.isNaN(data[2]) || data[2 /* X3 */] <-0.031795852f ? 
                            1.0f : 
                            0.0f)) : 
                    1.0f))) : 
         (data[27 /* X28 */] <-0.16785517f ? 
             (data[10 /* X11 */] <-0.4118269f ? 
                1.0f : 
                0.0f) : 
            0.0f));
    return pred;
  } // constant pool size = 70B, number of visited nodes = 17, static init size = 0B
}

class h2o_dt_10_Tree_0_class_1 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[3]) || data[3 /* X4 */] <0.29639092f ? 
         (data[19 /* X20 */] <-0.39988834f ? 
             (data[23 /* X24 */] <0.10067796f ? 
                 (Double.isNaN(data[11]) || data[11 /* X12 */] <0.10663433f ? 
                     (data[11 /* X12 */] <-0.34503615f ? 
                        0.0f : 
                        1.0f) : 
                    0.0f) : 
                0.0f) : 
            0.0f) : 
         (Double.isNaN(data[7]) || data[7 /* X8 */] <0.15481484f ? 
            1.0f : 
             (data[5 /* X6 */] <0.0824575f ? 
                1.0f : 
                0.0f)));
    return pred;
  } // constant pool size = 30B, number of visited nodes = 7, static init size = 0B
}

class h2o_dt_10_Tree_0_class_2 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[11]) || data[11 /* X12 */] <0.11689283f ? 
        0.0f : 
         (data[20 /* X21 */] <0.23067726f ? 
             (Double.isNaN(data[2]) || data[2 /* X3 */] <0.34719253f ? 
                1.0f : 
                0.0f) : 
            0.0f));
    return pred;
  } // constant pool size = 14B, number of visited nodes = 3, static init size = 0B
}

class h2o_dt_10_Tree_0_class_3 {
  static final double score0(double[] data) {
    double pred =      (data[32 /* X33 */] <-0.19921875f ? 
         (data[35 /* X36 */] <0.1572676f ? 
             (data[1 /* X2 */] <-0.20612775f ? 
                0.0f : 
                 (data[5 /* X6 */] <0.09835265f ? 
                    0.0f : 
                     (Double.isNaN(data[2]) || data[2 /* X3 */] <0.33377865f ? 
                        1.0f : 
                        0.0f))) : 
             (data[22 /* X23 */] <0.10372536f ? 
                 (Double.isNaN(data[2]) || data[2 /* X3 */] <0.14240015f ? 
                    1.0f : 
                    0.0f) : 
                 (Double.isNaN(data[8]) || data[8 /* X9 */] <1.040398f ? 
                     (Double.isNaN(data[7]) || data[7 /* X8 */] <0.35131156f ? 
                        0.0f : 
                        1.0f) : 
                     (data[0 /* X1 */] <0.29541972f ? 
                        0.0f : 
                        1.0f)))) : 
         (Double.isNaN(data[13]) || data[13 /* X14 */] <0.42887717f ? 
             (data[0 /* X1 */] <-0.43347532f ? 
                1.0f : 
                 (data[15 /* X16 */] <0.04116694f ? 
                     (data[0 /* X1 */] <-0.089914516f ? 
                        1.0f : 
                        0.0f) : 
                    0.0f)) : 
            1.0f));
    return pred;
  } // constant pool size = 58B, number of visited nodes = 14, static init size = 0B
}

class h2o_dt_10_Tree_0_class_4 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[35]) || data[35 /* X36 */] <0.1572676f ? 
         (Double.isNaN(data[14]) || data[14 /* X15 */] <0.31683603f ? 
             (Double.isNaN(data[34]) || data[34 /* X35 */] <0.14989693f ? 
                 (data[13 /* X14 */] <0.07661439f ? 
                    1.0f : 
                     (Double.isNaN(data[17]) || data[17 /* X18 */] <0.39900172f ? 
                        0.0f : 
                         (data[0 /* X1 */] <-0.01557902f ? 
                            0.0f : 
                            1.0f))) : 
                 (data[2 /* X3 */] <-0.04303788f ? 
                     (data[0 /* X1 */] <-0.10877304f ? 
                        0.0f : 
                        1.0f) : 
                     (data[29 /* X30 */] <0.109801136f ? 
                        1.0f : 
                        0.0f))) : 
             (data[33 /* X34 */] <-0.42553666f ? 
                0.0f : 
                 (data[12 /* X13 */] <0.24569999f ? 
                    0.0f : 
                    1.0f))) : 
         (Double.isNaN(data[25]) || data[25 /* X26 */] <-0.06177754f ? 
             (Double.isNaN(data[0]) || data[0 /* X1 */] <0.3340732f ? 
                1.0f : 
                 (data[1 /* X2 */] <-0.20589505f ? 
                    1.0f : 
                    0.0f)) : 
             (data[17 /* X18 */] <-0.086048886f ? 
                1.0f : 
                 (data[7 /* X8 */] <0.058360133f ? 
                    1.0f : 
                     (data[35 /* X36 */] <0.24636014f ? 
                         (Double.isNaN(data[0]) || data[0 /* X1 */] <0.06380092f ? 
                            0.0f : 
                            1.0f) : 
                        0.0f)))));
    return pred;
  } // constant pool size = 74B, number of visited nodes = 18, static init size = 0B
}

class h2o_dt_10_Tree_0_class_5 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[27]) || data[27 /* X28 */] <0.5183789f ? 
         (Double.isNaN(data[19]) || data[19 /* X20 */] <0.272891f ? 
             (data[14 /* X15 */] <0.08208819f ? 
                1.0f : 
                 (data[11 /* X12 */] <-0.59317803f ? 
                     (data[0 /* X1 */] <-0.027929958f ? 
                        0.0f : 
                        1.0f) : 
                    0.0f)) : 
             (data[28 /* X29 */] <0.284022f ? 
                1.0f : 
                 (data[5 /* X6 */] <0.10664187f ? 
                    1.0f : 
                    0.0f))) : 
         (Double.isNaN(data[31]) || data[31 /* X32 */] <0.14215098f ? 
            1.0f : 
             (data[22 /* X23 */] <0.21840884f ? 
                1.0f : 
                0.0f)));
    return pred;
  } // constant pool size = 38B, number of visited nodes = 9, static init size = 0B
}



