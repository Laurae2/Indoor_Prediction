/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:09:02.532+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DRFModel named h2o_dt_11

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_dt_11 > h2o_dt_11.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_dt_11.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_dt_11", algorithm="drf")
public class h2o_dt_11 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }

  public boolean isSupervised() { return true; }
  public int nfeatures() { return 36; }
  public int nclasses() { return 6; }

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_dt_11.VALUES;
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
    /* Label */ h2o_dt_11_ColInfo_36.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.2548076923076923,0.18269230769230768,0.057692307692307696,0.125,0.2548076923076923,0.125};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = {0.2548076923076923,0.18269230769230768,0.057692307692307696,0.125,0.2548076923076923,0.125};

  public h2o_dt_11() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(6223709780816446892L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    java.util.Arrays.fill(preds,0);
    h2o_dt_11_Forest_0.score0(data,preds);
    double sum = 0;
    for(int i=1; i<preds.length; i++) { sum += preds[i]; }
    if (sum>0) for(int i=1; i<preds.length; i++) { preds[i] /= sum; }
    preds[0] = hex.genmodel.GenModel.getPrediction(preds, PRIOR_CLASS_DISTRIB, data, 0.5);
    return preds;
  }
}
// The class representing training column names
class NamesHolder_h2o_dt_11 implements java.io.Serializable {
  public static final String[] VALUES = new String[36];
  static {
    NamesHolder_h2o_dt_11_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_dt_11_0 implements java.io.Serializable {
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
class h2o_dt_11_ColInfo_36 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_dt_11_ColInfo_36_0.fill(VALUES);
  }
  static final class h2o_dt_11_ColInfo_36_0 implements java.io.Serializable {
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

class h2o_dt_11_Forest_0 {
  public static void score0(double[] fdata, double[] preds) {
    preds[1] += h2o_dt_11_Tree_0_class_0.score0(fdata);
    preds[2] += h2o_dt_11_Tree_0_class_1.score0(fdata);
    preds[3] += h2o_dt_11_Tree_0_class_2.score0(fdata);
    preds[4] += h2o_dt_11_Tree_0_class_3.score0(fdata);
    preds[5] += h2o_dt_11_Tree_0_class_4.score0(fdata);
    preds[6] += h2o_dt_11_Tree_0_class_5.score0(fdata);
  }
}
class h2o_dt_11_Tree_0_class_0 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[32]) || data[32 /* X33 */] <-0.047319345f ? 
         (data[9 /* X10 */] <-0.5629341f ? 
             (Double.isNaN(data[2]) || data[2 /* X3 */] <0.17099866f ? 
                1.0f : 
                0.0f) : 
             (data[17 /* X18 */] <-0.5882706f ? 
                1.0f : 
                 (data[35 /* X36 */] <-0.255625f ? 
                     (data[31 /* X32 */] <0.102123864f ? 
                         (Double.isNaN(data[0]) || data[0 /* X1 */] <0.3980412f ? 
                            1.0f : 
                            0.0f) : 
                        0.0f) : 
                     (data[10 /* X11 */] <-0.69660777f ? 
                         (data[0 /* X1 */] <-0.12984568f ? 
                            0.0f : 
                            1.0f) : 
                        0.0f)))) : 
         (data[18 /* X19 */] <0.169726f ? 
             (data[24 /* X25 */] <-0.14330827f ? 
                 (data[35 /* X36 */] <-0.278125f ? 
                     (Double.isNaN(data[0]) || data[0 /* X1 */] <0.28136075f ? 
                        0.0f : 
                        1.0f) : 
                    1.0f) : 
                0.0f) : 
             (Double.isNaN(data[21]) || data[21 /* X22 */] <0.1859464f ? 
                 (Double.isNaN(data[2]) || data[2 /* X3 */] <0.18335566f ? 
                     (data[33 /* X34 */] <-0.25850773f ? 
                         (Double.isNaN(data[0]) || data[0 /* X1 */] <0.11624268f ? 
                            0.0f : 
                            1.0f) : 
                        1.0f) : 
                     (Double.isNaN(data[3]) || data[3 /* X4 */] <0.2087627f ? 
                        0.0f : 
                        1.0f)) : 
                 (Double.isNaN(data[7]) || data[7 /* X8 */] <0.17276295f ? 
                     (data[4 /* X5 */] <0.13960025f ? 
                        1.0f : 
                        0.0f) : 
                    1.0f))));
    return pred;
  } // constant pool size = 82B, number of visited nodes = 20, static init size = 0B
}

class h2o_dt_11_Tree_0_class_1 {
  static final double score0(double[] data) {
    double pred =      (data[19 /* X20 */] <-0.4811662f ? 
         (Double.isNaN(data[23]) || data[23 /* X24 */] <0.10524856f ? 
             (data[12 /* X13 */] <0.16658485f ? 
                0.0f : 
                1.0f) : 
             (data[14 /* X15 */] <0.19528002f ? 
                1.0f : 
                 (Double.isNaN(data[1]) || data[1 /* X2 */] <0.21507634f ? 
                    0.0f : 
                    1.0f))) : 
         (Double.isNaN(data[2]) || data[2 /* X3 */] <0.45528913f ? 
             (data[27 /* X28 */] <-0.44542167f ? 
                 (data[3 /* X4 */] <-0.26063165f ? 
                    0.0f : 
                    1.0f) : 
                 (data[35 /* X36 */] <-0.9401597f ? 
                     (data[0 /* X1 */] <-0.026762143f ? 
                        1.0f : 
                        0.0f) : 
                     (data[34 /* X35 */] <-0.96875f ? 
                         (Double.isNaN(data[1]) || data[1 /* X2 */] <-0.05038032f ? 
                            0.0f : 
                            1.0f) : 
                        0.0f))) : 
             (data[3 /* X4 */] <-0.62166727f ? 
                0.0f : 
                1.0f)));
    return pred;
  } // constant pool size = 54B, number of visited nodes = 13, static init size = 0B
}

class h2o_dt_11_Tree_0_class_2 {
  static final double score0(double[] data) {
    double pred =      (data[35 /* X36 */] <-0.36132812f ? 
         (data[18 /* X19 */] <-0.07598811f ? 
             (Double.isNaN(data[4]) || data[4 /* X5 */] <0.1942089f ? 
                1.0f : 
                0.0f) : 
             (data[4 /* X5 */] <0.10091396f ? 
                1.0f : 
                 (Double.isNaN(data[0]) || data[0 /* X1 */] <0.7219959f ? 
                     (Double.isNaN(data[21]) || data[21 /* X22 */] <0.2318404f ? 
                         (data[26 /* X27 */] <0.10842864f ? 
                             (data[2 /* X3 */] <-0.26530087f ? 
                                1.0f : 
                                0.0f) : 
                            0.0f) : 
                         (data[0 /* X1 */] <0.35159546f ? 
                            0.0f : 
                            1.0f)) : 
                    1.0f))) : 
        0.0f);
    return pred;
  } // constant pool size = 38B, number of visited nodes = 9, static init size = 0B
}

class h2o_dt_11_Tree_0_class_3 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[19]) || data[19 /* X20 */] <0.10111102f ? 
         (Double.isNaN(data[22]) || data[22 /* X23 */] <0.38338542f ? 
             (Double.isNaN(data[28]) || data[28 /* X29 */] <0.41573414f ? 
                0.0f : 
                1.0f) : 
            1.0f) : 
         (Double.isNaN(data[11]) || data[11 /* X12 */] <-0.42544553f ? 
             (data[2 /* X3 */] <-0.4532986f ? 
                1.0f : 
                0.0f) : 
             (Double.isNaN(data[29]) || data[29 /* X30 */] <0.23498286f ? 
                 (data[3 /* X4 */] <-0.41988644f ? 
                     (data[0 /* X1 */] <0.15179193f ? 
                        1.0f : 
                        0.0f) : 
                    1.0f) : 
                 (data[0 /* X1 */] <0.20567451f ? 
                    1.0f : 
                    0.0f))));
    return pred;
  } // constant pool size = 38B, number of visited nodes = 9, static init size = 0B
}

class h2o_dt_11_Tree_0_class_4 {
  static final double score0(double[] data) {
    double pred =      (data[34 /* X35 */] <-0.119140625f ? 
         (Double.isNaN(data[23]) || data[23 /* X24 */] <0.19734105f ? 
             (data[31 /* X32 */] <0.034999687f ? 
                 (data[0 /* X1 */] <0.017282447f ? 
                    0.0f : 
                    1.0f) : 
                0.0f) : 
             (data[0 /* X1 */] <-0.25799623f ? 
                0.0f : 
                1.0f)) : 
         (data[15 /* X16 */] <0.058502894f ? 
             (data[0 /* X1 */] <-0.14432254f ? 
                0.0f : 
                1.0f) : 
             (data[20 /* X21 */] <0.16725914f ? 
                0.0f : 
                 (data[9 /* X10 */] <-0.18599069f ? 
                     (Double.isNaN(data[12]) || data[12 /* X13 */] <0.2865993f ? 
                        1.0f : 
                         (data[4 /* X5 */] <0.15414715f ? 
                            1.0f : 
                            0.0f)) : 
                     (Double.isNaN(data[7]) || data[7 /* X8 */] <0.18278603f ? 
                         (data[1 /* X2 */] <-0.30027726f ? 
                             (Double.isNaN(data[5]) || data[5 /* X6 */] <0.2290891f ? 
                                1.0f : 
                                0.0f) : 
                             (Double.isNaN(data[0]) || data[0 /* X1 */] <0.5011527f ? 
                                0.0f : 
                                1.0f)) : 
                         (Double.isNaN(data[25]) || data[25 /* X26 */] <0.20269917f ? 
                             (data[19 /* X20 */] <0.20757972f ? 
                                 (Double.isNaN(data[1]) || data[1 /* X2 */] <-0.13054283f ? 
                                    0.0f : 
                                    1.0f) : 
                                1.0f) : 
                            0.0f))))));
    return pred;
  } // constant pool size = 74B, number of visited nodes = 18, static init size = 0B
}

class h2o_dt_11_Tree_0_class_5 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[27]) || data[27 /* X28 */] <0.3787111f ? 
         (Double.isNaN(data[16]) || data[16 /* X17 */] <0.34592894f ? 
             (data[6 /* X7 */] <0.050165128f ? 
                1.0f : 
                 (Double.isNaN(data[19]) || data[19 /* X20 */] <0.5012791f ? 
                    0.0f : 
                     (Double.isNaN(data[3]) || data[3 /* X4 */] <-0.30539718f ? 
                        0.0f : 
                        1.0f))) : 
             (data[0 /* X1 */] <0.1751089f ? 
                0.0f : 
                1.0f)) : 
         (Double.isNaN(data[6]) || data[6 /* X7 */] <0.22306623f ? 
             (Double.isNaN(data[19]) || data[19 /* X20 */] <0.87760156f ? 
                 (data[10 /* X11 */] <-0.08966015f ? 
                     (data[0 /* X1 */] <0.005420867f ? 
                        0.0f : 
                        1.0f) : 
                    1.0f) : 
                0.0f) : 
            0.0f));
    return pred;
  } // constant pool size = 42B, number of visited nodes = 10, static init size = 0B
}



