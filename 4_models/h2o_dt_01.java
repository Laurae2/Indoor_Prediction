/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:40:56.323+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DRFModel named h2o_dt_01

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_dt_01 > h2o_dt_01.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_dt_01.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_dt_01", algorithm="drf")
public class h2o_dt_01 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }

  public boolean isSupervised() { return true; }
  public int nfeatures() { return 13; }
  public int nclasses() { return 5; }

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_dt_01.VALUES;
  // Number of output classes included in training data response column.
  public static final int NCLASSES = 5;

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
    /* Label */ h2o_dt_01_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.25,0.25,0.125,0.25,0.125};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = {0.25,0.25,0.125,0.25,0.125};

  public h2o_dt_01() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(7045936541977875152L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    java.util.Arrays.fill(preds,0);
    h2o_dt_01_Forest_0.score0(data,preds);
    double sum = 0;
    for(int i=1; i<preds.length; i++) { sum += preds[i]; }
    if (sum>0) for(int i=1; i<preds.length; i++) { preds[i] /= sum; }
    preds[0] = hex.genmodel.GenModel.getPrediction(preds, PRIOR_CLASS_DISTRIB, data, 0.5);
    return preds;
  }
}
// The class representing training column names
class NamesHolder_h2o_dt_01 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_dt_01_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_dt_01_0 implements java.io.Serializable {
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
class h2o_dt_01_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    h2o_dt_01_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_dt_01_ColInfo_13_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "4";
      sa[3] = "5";
      sa[4] = "6";
    }
  }
}

class h2o_dt_01_Forest_0 {
  public static void score0(double[] fdata, double[] preds) {
    preds[1] += h2o_dt_01_Tree_0_class_0.score0(fdata);
    preds[2] += h2o_dt_01_Tree_0_class_1.score0(fdata);
    preds[3] += h2o_dt_01_Tree_0_class_2.score0(fdata);
    preds[4] += h2o_dt_01_Tree_0_class_3.score0(fdata);
    preds[5] += h2o_dt_01_Tree_0_class_4.score0(fdata);
  }
}
class h2o_dt_01_Tree_0_class_0 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[1]) || data[1 /* X12 */] <-0.14438298f ? 
         (data[7 /* X21 */] <0.11131099f ? 
            1.0f : 
            0.0f) : 
         (data[6 /* X20 */] <-0.48962033f ? 
             (Double.isNaN(data[8]) || data[8 /* X23 */] <0.21108326f ? 
                 (data[0 /* X5 */] <0.11573713f ? 
                    1.0f : 
                    0.0f) : 
                 (data[1 /* X12 */] <0.3578141f ? 
                    0.0f : 
                    1.0f)) : 
             (Double.isNaN(data[10]) || data[10 /* X28 */] <0.0256477f ? 
                 (data[2 /* X15 */] <0.19373427f ? 
                    0.0f : 
                    1.0f) : 
                0.0f)));
    return pred;
  } // constant pool size = 34B, number of visited nodes = 8, static init size = 0B
}

class h2o_dt_01_Tree_0_class_1 {
  static final double score0(double[] data) {
    double pred =      (data[6 /* X20 */] <-0.49490413f ? 
         (Double.isNaN(data[8]) || data[8 /* X23 */] <0.21093145f ? 
             (data[0 /* X5 */] <0.11573713f ? 
                0.0f : 
                1.0f) : 
             (data[0 /* X5 */] <0.19387387f ? 
                 (Double.isNaN(data[0]) || data[0 /* X5 */] <0.18499163f ? 
                    1.0f : 
                    0.0f) : 
                0.0f)) : 
        0.0f);
    return pred;
  } // constant pool size = 22B, number of visited nodes = 5, static init size = 0B
}

class h2o_dt_01_Tree_0_class_2 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[8]) || data[8 /* X23 */] <0.39608935f ? 
         (Double.isNaN(data[6]) || data[6 /* X20 */] <0.26279598f ? 
             (data[3 /* X17 */] <-0.7687134f ? 
                 (data[1 /* X12 */] <-0.6924453f ? 
                    0.0f : 
                    1.0f) : 
                 (Double.isNaN(data[12]) || data[12 /* X35 */] <0.35714f ? 
                    0.0f : 
                     (data[7 /* X21 */] <0.19428456f ? 
                        1.0f : 
                        0.0f))) : 
             (Double.isNaN(data[1]) || data[1 /* X12 */] <-0.44988576f ? 
                 (Double.isNaN(data[5]) || data[5 /* X19 */] <0.47398478f ? 
                    0.0f : 
                    1.0f) : 
                1.0f)) : 
         (data[2 /* X15 */] <0.23919171f ? 
            0.0f : 
            1.0f));
    return pred;
  } // constant pool size = 38B, number of visited nodes = 9, static init size = 0B
}

class h2o_dt_01_Tree_0_class_3 {
  static final double score0(double[] data) {
    double pred =      (data[6 /* X20 */] <-0.1451151f ? 
        0.0f : 
         (Double.isNaN(data[10]) || data[10 /* X28 */] <0.9916381f ? 
             (Double.isNaN(data[1]) || data[1 /* X12 */] <-0.19146678f ? 
                 (data[4 /* X18 */] <-0.036982715f ? 
                    1.0f : 
                     (Double.isNaN(data[1]) || data[1 /* X12 */] <-0.63511676f ? 
                         (Double.isNaN(data[0]) || data[0 /* X5 */] <0.24417482f ? 
                            1.0f : 
                             (Double.isNaN(data[1]) || data[1 /* X12 */] <-0.73339427f ? 
                                0.0f : 
                                1.0f)) : 
                         (data[0 /* X5 */] <0.18538447f ? 
                             (Double.isNaN(data[2]) || data[2 /* X15 */] <0.25889418f ? 
                                1.0f : 
                                0.0f) : 
                            0.0f))) : 
                 (data[0 /* X5 */] <0.13475944f ? 
                    1.0f : 
                     (data[11 /* X34 */] <-0.4234375f ? 
                        1.0f : 
                        0.0f))) : 
            0.0f));
    return pred;
  } // constant pool size = 46B, number of visited nodes = 11, static init size = 0B
}

class h2o_dt_01_Tree_0_class_4 {
  static final double score0(double[] data) {
    double pred =      (Double.isNaN(data[10]) || data[10 /* X28 */] <1.0273732f ? 
         (Double.isNaN(data[3]) || data[3 /* X17 */] <0.30743295f ? 
             (Double.isNaN(data[10]) || data[10 /* X28 */] <0.46964133f ? 
                0.0f : 
                 (data[10 /* X28 */] <0.55854744f ? 
                    1.0f : 
                    0.0f)) : 
             (data[0 /* X5 */] <0.17892851f ? 
                0.0f : 
                1.0f)) : 
        1.0f);
    return pred;
  } // constant pool size = 22B, number of visited nodes = 5, static init size = 0B
}



