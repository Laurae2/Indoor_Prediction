/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T13:48:55.912+01:00
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
  public String getUUID() { return Long.toString(339381839020051464L); }

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
    double pred =      (data[16 /* X17 */] <-0.24765773f ? 
         (Double.isNaN(data[2]) || data[2 /* X3 */] <0.08497282f ? 
             (data[32 /* X33 */] <-0.3027347f ? 
                 (Double.isNaN(data[7]) || data[7 /* X8 */] <0.113855265f ? 
                     (data[0 /* X1 */] <-0.07256024f ? 
                        1.0f : 
                        0.0f) : 
                    1.0f) : 
                 (data[19 /* X20 */] <-0.30153084f ? 
                     (data[0 /* X1 */] <-0.14590798f ? 
                        1.0f : 
                        0.0f) : 
                     (data[21 /* X22 */] <0.10958655f ? 
                        0.0f : 
                        1.0f))) : 
             (data[17 /* X18 */] <-0.4836999f ? 
                1.0f : 
                0.0f)) : 
         (data[15 /* X16 */] <0.048957206f ? 
             (data[32 /* X33 */] <-0.11185906f ? 
                0.0f : 
                1.0f) : 
             (Double.isNaN(data[21]) || data[21 /* X22 */] <0.27610737f ? 
                 (data[12 /* X13 */] <0.08346164f ? 
                    1.0f : 
                     (Double.isNaN(data[19]) || data[19 /* X20 */] <0.6848716f ? 
                         (Double.isNaN(data[24]) || data[24 /* X25 */] <0.6580089f ? 
                             (data[3 /* X4 */] <-0.80411345f ? 
                                 (data[0 /* X1 */] <-0.087022044f ? 
                                    1.0f : 
                                    0.0f) : 
                                0.0f) : 
                             (data[0 /* X1 */] <0.18217583f ? 
                                0.0f : 
                                1.0f)) : 
                        1.0f)) : 
                 (data[7 /* X8 */] <0.06456461f ? 
                    1.0f : 
                     (data[14 /* X15 */] <0.10140836f ? 
                        1.0f : 
                        0.0f)))));
    return pred;
  } // constant pool size = 82B, number of visited nodes = 20, static init size = 0B
}

class h2o_dt_11_Tree_0_class_1 {
  static final double score0(double[] data) {
    double pred =      (data[3 /* X4 */] <-0.49490413f ? 
         (Double.isNaN(data[7]) || data[7 /* X8 */] <0.104305856f ? 
             (data[11 /* X12 */] <-0.55065507f ? 
                0.0f : 
                1.0f) : 
             (Double.isNaN(data[16]) || data[16 /* X17 */] <0.2655735f ? 
                0.0f : 
                1.0f)) : 
         (Double.isNaN(data[2]) || data[2 /* X3 */] <0.66561913f ? 
             (data[34 /* X35 */] <-0.859375f ? 
                 (Double.isNaN(data[20]) || data[20 /* X21 */] <0.181024f ? 
                    0.0f : 
                     (data[28 /* X29 */] <0.17172639f ? 
                        1.0f : 
                         (data[20 /* X21 */] <0.21482664f ? 
                            1.0f : 
                            0.0f))) : 
                 (Double.isNaN(data[18]) || data[18 /* X19 */] <0.9757321f ? 
                     (data[35 /* X36 */] <-0.96953124f ? 
                         (Double.isNaN(data[2]) || data[2 /* X3 */] <0.10047016f ? 
                            0.0f : 
                            1.0f) : 
                         (Double.isNaN(data[21]) || data[21 /* X22 */] <0.22931674f ? 
                            0.0f : 
                             (Double.isNaN(data[32]) || data[32 /* X33 */] <0.718181f ? 
                                 (data[30 /* X31 */] <0.13963744f ? 
                                     (data[0 /* X1 */] <0.37308076f ? 
                                        1.0f : 
                                        0.0f) : 
                                    0.0f) : 
                                1.0f))) : 
                    1.0f)) : 
            1.0f));
    return pred;
  } // constant pool size = 66B, number of visited nodes = 16, static init size = 0B
}

class h2o_dt_11_Tree_0_class_2 {
  static final double score0(double[] data) {
    double pred =      (data[34 /* X35 */] <-0.5859375f ? 
         (Double.isNaN(data[30]) || data[30 /* X31 */] <0.17622836f ? 
             (Double.isNaN(data[11]) || data[11 /* X12 */] <-0.07575571f ? 
                 (Double.isNaN(data[15]) || data[15 /* X16 */] <0.13259223f ? 
                    0.0f : 
                    1.0f) : 
                 (Double.isNaN(data[10]) || data[10 /* X11 */] <0.04533548f ? 
                    1.0f : 
                    0.0f)) : 
            0.0f) : 
         (Double.isNaN(data[10]) || data[10 /* X11 */] <0.5164477f ? 
            0.0f : 
             (Double.isNaN(data[1]) || data[1 /* X2 */] <-0.12719479f ? 
                0.0f : 
                1.0f)));
    return pred;
  } // constant pool size = 30B, number of visited nodes = 7, static init size = 0B
}

class h2o_dt_11_Tree_0_class_3 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[0]) || data[0 /* X1 */] <0.5320536f ? 
         (Double.isNaN(data[4]) || data[4 /* X5 */] <0.40021002f ? 
             (data[10 /* X11 */] <-0.6006276f ? 
                 (Double.isNaN(data[28]) || data[28 /* X29 */] <0.1675908f ? 
                    0.0f : 
                    1.0f) : 
                 (Double.isNaN(data[25]) || data[25 /* X26 */] <0.61711556f ? 
                     (data[33 /* X34 */] <-0.84375f ? 
                         (Double.isNaN(data[13]) || data[13 /* X14 */] <0.14928493f ? 
                            0.0f : 
                            1.0f) : 
                         (Double.isNaN(data[5]) || data[5 /* X6 */] <0.35793528f ? 
                             (data[33 /* X34 */] <-0.564f ? 
                                 (Double.isNaN(data[16]) || data[16 /* X17 */] <0.16129732f ? 
                                    0.0f : 
                                     (data[3 /* X4 */] <-0.4213051f ? 
                                        0.0f : 
                                        1.0f)) : 
                                 (Double.isNaN(data[12]) || data[12 /* X13 */] <0.30439934f ? 
                                    0.0f : 
                                     (data[15 /* X16 */] <0.16735698f ? 
                                         (data[0 /* X1 */] <0.05779375f ? 
                                            1.0f : 
                                            0.0f) : 
                                        0.0f))) : 
                             (Double.isNaN(data[35]) || data[35 /* X36 */] <-0.22f ? 
                                0.0f : 
                                1.0f))) : 
                    1.0f)) : 
            1.0f) : 
         (Double.isNaN(data[19]) || data[19 /* X20 */] <0.0844181f ? 
             (data[18 /* X19 */] <-0.038061086f ? 
                1.0f : 
                0.0f) : 
            1.0f));
    return pred;
  } // constant pool size = 70B, number of visited nodes = 17, static init size = 0B
}

class h2o_dt_11_Tree_0_class_4 {
  static final double score0(double[] data) {
    double pred =      (data[3 /* X4 */] <-0.33427593f ? 
        0.0f : 
         (Double.isNaN(data[35]) || data[35 /* X36 */] <0.49106446f ? 
             (Double.isNaN(data[3]) || data[3 /* X4 */] <0.7973617f ? 
                 (Double.isNaN(data[34]) || data[34 /* X35 */] <0.5078125f ? 
                     (Double.isNaN(data[13]) || data[13 /* X14 */] <0.24066135f ? 
                         (data[3 /* X4 */] <-0.2972145f ? 
                            1.0f : 
                             (Double.isNaN(data[31]) || data[31 /* X32 */] <0.15213265f ? 
                                 (data[21 /* X22 */] <0.11813473f ? 
                                     (Double.isNaN(data[1]) || data[1 /* X2 */] <-0.032214645f ? 
                                         (Double.isNaN(data[10]) || data[10 /* X11 */] <-0.27523613f ? 
                                            1.0f : 
                                            0.0f) : 
                                        0.0f) : 
                                     (Double.isNaN(data[30]) || data[30 /* X31 */] <0.29275885f ? 
                                         (Double.isNaN(data[35]) || data[35 /* X36 */] <0.33f ? 
                                             (Double.isNaN(data[4]) || data[4 /* X5 */] <0.3331564f ? 
                                                0.0f : 
                                                 (Double.isNaN(data[0]) || data[0 /* X1 */] <-0.4219831f ? 
                                                    0.0f : 
                                                    1.0f)) : 
                                             (data[0 /* X1 */] <0.43920326f ? 
                                                1.0f : 
                                                0.0f)) : 
                                        1.0f)) : 
                                 (data[8 /* X9 */] <-0.047422938f ? 
                                    0.0f : 
                                     (data[3 /* X4 */] <0.007829713f ? 
                                        0.0f : 
                                        1.0f)))) : 
                         (Double.isNaN(data[11]) || data[11 /* X12 */] <0.4224297f ? 
                             (Double.isNaN(data[27]) || data[27 /* X28 */] <-0.2827858f ? 
                                 (Double.isNaN(data[31]) || data[31 /* X32 */] <0.22315744f ? 
                                    1.0f : 
                                    0.0f) : 
                                 (Double.isNaN(data[16]) || data[16 /* X17 */] <-0.09082289f ? 
                                    0.0f : 
                                    1.0f)) : 
                             (data[10 /* X11 */] <-0.21999654f ? 
                                1.0f : 
                                0.0f))) : 
                     (Double.isNaN(data[19]) || data[19 /* X20 */] <0.041116916f ? 
                        1.0f : 
                        0.0f)) : 
                1.0f) : 
            1.0f));
    return pred;
  } // constant pool size = 94B, number of visited nodes = 23, static init size = 0B
}

class h2o_dt_11_Tree_0_class_5 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[11]) || data[11 /* X12 */] <1.0286022f ? 
         (data[11 /* X12 */] <-0.5208542f ? 
             (Double.isNaN(data[15]) || data[15 /* X16 */] <0.14567566f ? 
                1.0f : 
                0.0f) : 
             (Double.isNaN(data[1]) || data[1 /* X2 */] <0.26533395f ? 
                 (Double.isNaN(data[7]) || data[7 /* X8 */] <0.554828f ? 
                     (data[11 /* X12 */] <-0.42007756f ? 
                         (Double.isNaN(data[17]) || data[17 /* X18 */] <0.03184976f ? 
                            0.0f : 
                             (Double.isNaN(data[0]) || data[0 /* X1 */] <0.29868397f ? 
                                1.0f : 
                                0.0f)) : 
                         (Double.isNaN(data[11]) || data[11 /* X12 */] <0.49705568f ? 
                            0.0f : 
                             (data[5 /* X6 */] <0.22450197f ? 
                                1.0f : 
                                0.0f))) : 
                    1.0f) : 
                 (data[2 /* X3 */] <-0.0078026224f ? 
                    0.0f : 
                     (Double.isNaN(data[1]) || data[1 /* X2 */] <0.42258754f ? 
                        1.0f : 
                        0.0f)))) : 
        1.0f);
    return pred;
  } // constant pool size = 50B, number of visited nodes = 12, static init size = 0B
}



