/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T13:48:38.162+01:00
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
  public String getUUID() { return Long.toString(-3223676372050974960L); }

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
    double pred =      (data[16 /* X17 */] <-0.29178956f ? 
         (Double.isNaN(data[11]) || data[11 /* X12 */] <0.004749425f ? 
             (Double.isNaN(data[34]) || data[34 /* X35 */] <0.16051406f ? 
                 (Double.isNaN(data[15]) || data[15 /* X16 */] <0.114858076f ? 
                     (Double.isNaN(data[7]) || data[7 /* X8 */] <0.13533908f ? 
                        1.0f : 
                        0.0f) : 
                     (data[16 /* X17 */] <-0.7012892f ? 
                        1.0f : 
                        0.0f)) : 
                 (Double.isNaN(data[4]) || data[4 /* X5 */] <0.19779481f ? 
                    0.0f : 
                    1.0f)) : 
            0.0f) : 
         (data[31 /* X32 */] <0.094746366f ? 
             (Double.isNaN(data[23]) || data[23 /* X24 */] <0.063911766f ? 
                 (data[11 /* X12 */] <-0.3453262f ? 
                    1.0f : 
                    0.0f) : 
                 (Double.isNaN(data[24]) || data[24 /* X25 */] <-0.118098326f ? 
                     (Double.isNaN(data[23]) || data[23 /* X24 */] <0.09649334f ? 
                        1.0f : 
                        0.0f) : 
                     (Double.isNaN(data[1]) || data[1 /* X2 */] <0.09671313f ? 
                        0.0f : 
                        1.0f))) : 
             (Double.isNaN(data[34]) || data[34 /* X35 */] <0.39773f ? 
                 (Double.isNaN(data[21]) || data[21 /* X22 */] <0.3505687f ? 
                    0.0f : 
                    1.0f) : 
                1.0f)));
    return pred;
  } // constant pool size = 62B, number of visited nodes = 15, static init size = 0B
}

class h2o_dt_10_Tree_0_class_1 {
  static final double score0(double[] data) {
    double pred =      (data[3 /* X4 */] <-0.49490413f ? 
         (Double.isNaN(data[7]) || data[7 /* X8 */] <0.1039355f ? 
            1.0f : 
             (Double.isNaN(data[16]) || data[16 /* X17 */] <0.21538177f ? 
                0.0f : 
                1.0f)) : 
         (data[19 /* X20 */] <-0.40358394f ? 
             (Double.isNaN(data[27]) || data[27 /* X28 */] <-0.15512678f ? 
                0.0f : 
                 (Double.isNaN(data[11]) || data[11 /* X12 */] <0.10591921f ? 
                     (data[33 /* X34 */] <-0.092547655f ? 
                         (Double.isNaN(data[4]) || data[4 /* X5 */] <0.16655986f ? 
                            0.0f : 
                            1.0f) : 
                        1.0f) : 
                    0.0f)) : 
             (data[10 /* X11 */] <-1.1598185f ? 
                1.0f : 
                0.0f)));
    return pred;
  } // constant pool size = 38B, number of visited nodes = 9, static init size = 0B
}

class h2o_dt_10_Tree_0_class_2 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[11]) || data[11 /* X12 */] <0.123157196f ? 
        0.0f : 
         (data[30 /* X31 */] <0.15246508f ? 
            1.0f : 
             (data[22 /* X23 */] <0.13612442f ? 
                1.0f : 
                 (data[12 /* X13 */] <0.090944886f ? 
                    1.0f : 
                    0.0f))));
    return pred;
  } // constant pool size = 18B, number of visited nodes = 4, static init size = 0B
}

class h2o_dt_10_Tree_0_class_3 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[13]) || data[13 /* X14 */] <0.29994604f ? 
         (data[25 /* X26 */] <-0.61987156f ? 
             (data[2 /* X3 */] <-0.0576773f ? 
                0.0f : 
                1.0f) : 
             (data[0 /* X1 */] <-0.65710235f ? 
                1.0f : 
                 (data[7 /* X8 */] <0.029081022f ? 
                     (data[1 /* X2 */] <-0.14137231f ? 
                        0.0f : 
                        1.0f) : 
                     (data[33 /* X34 */] <-0.84375f ? 
                         (Double.isNaN(data[8]) || data[8 /* X9 */] <0.5250176f ? 
                            0.0f : 
                            1.0f) : 
                         (Double.isNaN(data[4]) || data[4 /* X5 */] <0.4076f ? 
                             (data[0 /* X1 */] <-0.36209804f ? 
                                 (Double.isNaN(data[0]) || data[0 /* X1 */] <-0.5069239f ? 
                                    0.0f : 
                                    1.0f) : 
                                 (data[8 /* X9 */] <-0.18079695f ? 
                                     (data[15 /* X16 */] <0.047946036f ? 
                                        1.0f : 
                                        0.0f) : 
                                     (Double.isNaN(data[12]) || data[12 /* X13 */] <0.3021601f ? 
                                        0.0f : 
                                         (Double.isNaN(data[4]) || data[4 /* X5 */] <0.1640411f ? 
                                            0.0f : 
                                            1.0f)))) : 
                            1.0f))))) : 
         (data[27 /* X28 */] <-0.3227966f ? 
            0.0f : 
            1.0f));
    return pred;
  } // constant pool size = 66B, number of visited nodes = 16, static init size = 0B
}

class h2o_dt_10_Tree_0_class_4 {
  static final double score0(double[] data) {
    double pred =      (data[3 /* X4 */] <-0.17047745f ? 
         (data[10 /* X11 */] <-0.48227727f ? 
             (Double.isNaN(data[1]) || data[1 /* X2 */] <-0.019273207f ? 
                1.0f : 
                0.0f) : 
            0.0f) : 
         (Double.isNaN(data[34]) || data[34 /* X35 */] <0.41055903f ? 
             (data[27 /* X28 */] <-0.2530646f ? 
                 (Double.isNaN(data[11]) || data[11 /* X12 */] <0.99924976f ? 
                     (data[34 /* X35 */] <-0.92578095f ? 
                        0.0f : 
                         (Double.isNaN(data[34]) || data[34 /* X35 */] <-0.39538875f ? 
                             (data[5 /* X6 */] <0.1585115f ? 
                                0.0f : 
                                1.0f) : 
                             (Double.isNaN(data[1]) || data[1 /* X2 */] <-0.17532824f ? 
                                 (data[5 /* X6 */] <0.1951102f ? 
                                    0.0f : 
                                    1.0f) : 
                                0.0f))) : 
                    0.0f) : 
                 (Double.isNaN(data[35]) || data[35 /* X36 */] <0.26525033f ? 
                     (Double.isNaN(data[26]) || data[26 /* X27 */] <0.64143986f ? 
                         (data[13 /* X14 */] <0.084171586f ? 
                            1.0f : 
                             (Double.isNaN(data[21]) || data[21 /* X22 */] <0.30616936f ? 
                                0.0f : 
                                 (data[0 /* X1 */] <0.19671215f ? 
                                    1.0f : 
                                    0.0f))) : 
                        1.0f) : 
                     (data[5 /* X6 */] <0.15117627f ? 
                        1.0f : 
                         (data[1 /* X2 */] <-0.41999775f ? 
                            1.0f : 
                            0.0f)))) : 
             (Double.isNaN(data[0]) || data[0 /* X1 */] <0.32894725f ? 
                1.0f : 
                0.0f)));
    return pred;
  } // constant pool size = 78B, number of visited nodes = 19, static init size = 0B
}

class h2o_dt_10_Tree_0_class_5 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[10]) || data[10 /* X11 */] <0.28560278f ? 
         (Double.isNaN(data[11]) || data[11 /* X12 */] <1.0376076f ? 
             (Double.isNaN(data[27]) || data[27 /* X28 */] <0.5451083f ? 
                 (data[14 /* X15 */] <0.083287686f ? 
                    1.0f : 
                     (Double.isNaN(data[16]) || data[16 /* X17 */] <0.6405569f ? 
                         (Double.isNaN(data[0]) || data[0 /* X1 */] <0.5504905f ? 
                             (Double.isNaN(data[11]) || data[11 /* X12 */] <0.50400597f ? 
                                0.0f : 
                                 (data[5 /* X6 */] <0.24168332f ? 
                                    1.0f : 
                                    0.0f)) : 
                             (data[4 /* X5 */] <0.15132283f ? 
                                1.0f : 
                                0.0f)) : 
                         (data[0 /* X1 */] <0.15186764f ? 
                            0.0f : 
                            1.0f))) : 
                 (data[19 /* X20 */] <0.3182958f ? 
                    1.0f : 
                    0.0f)) : 
            1.0f) : 
         (data[23 /* X24 */] <0.12935866f ? 
             (data[0 /* X1 */] <0.14177151f ? 
                1.0f : 
                0.0f) : 
            1.0f));
    return pred;
  } // constant pool size = 54B, number of visited nodes = 13, static init size = 0B
}


