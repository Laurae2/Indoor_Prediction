/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:41:21.264+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_16x16x6_ReLU_02

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_16x16x6_ReLU_02 > h2o_nn_16x16x6_ReLU_02.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_16x16x6_ReLU_02.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_16x16x6_ReLU_02", algorithm="deeplearning")
public class h2o_nn_16x16x6_ReLU_02 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }
  public boolean isSupervised() { return true; }
  public int nfeatures() { return 13; }
  public int nclasses() { return 5; }
  // Thread-local storage for input neuron activation values.
  final double[] NUMS = new double[13];
  static class NORMMUL implements java.io.Serializable {
    public static final double[] VALUES = null;
}
  static class NORMSUB implements java.io.Serializable {
    public static final double[] VALUES = null;
}
  // Workspace for categorical offsets.
  public static final int[] CATOFFSETS = {0};
  // Number of neurons for each layer.
  public static final int[] NEURONS = {13,16,16,5};
    // Thread-local storage for neuron activation values.
    final double[][] ACTIVATION = new double[][] {
      /* Input */ h2o_nn_16x16x6_ReLU_02_Activation_0.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_02_Activation_1.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_02_Activation_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_ReLU_02_Activation_3.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_16x16x6_ReLU_02_Bias_0.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_02_Bias_1.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_02_Bias_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_ReLU_02_Bias_3.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_16x16x6_ReLU_02_Weight_0.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_02_Weight_1.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_02_Weight_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_ReLU_02_Weight_3.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_16x16x6_ReLU_02.VALUES;
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
    /* Label */ h2o_nn_16x16x6_ReLU_02_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.25,0.25,0.125,0.25,0.125};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_16x16x6_ReLU_02() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(572969830790473152L); }

  // Pass in data in a double[], pre-aligned to the Model's requirements.
  // Jam predictions into the preds[] array; preds[0] is reserved for the
  // main prediction (class for classifiers or value for regression),
  // and remaining columns hold a probability distribution for classifiers.
  public final double[] score0( double[] data, double[] preds ) {
    java.util.Arrays.fill(preds,0);
    java.util.Arrays.fill(NUMS,0);
    int i = 0, ncats = 0;
    final int n = data.length;
    for(; i<n; ++i) {
      NUMS[i] = Double.isNaN(data[i]) ? 0 : data[i];
    }
    java.util.Arrays.fill(ACTIVATION[0],0);
    for (i=0; i<NUMS.length; ++i) {
      ACTIVATION[0][CATOFFSETS[CATOFFSETS.length-1] + i] = Double.isNaN(NUMS[i]) ? 0 : NUMS[i];
    }
    for (i=1; i<ACTIVATION.length; ++i) {
      java.util.Arrays.fill(ACTIVATION[i],0);
      int cols = ACTIVATION[i-1].length;
      int rows = ACTIVATION[i].length;
      int extra=cols-cols%8;
      int multiple = (cols/8)*8-1;
      int idx = 0;
      float[] a = WEIGHT[i];
      double[] x = ACTIVATION[i-1];
      double[] y = BIAS[i];
      double[] res = ACTIVATION[i];
      for (int row=0; row<rows; ++row) {
        double psum0 = 0, psum1 = 0, psum2 = 0, psum3 = 0, psum4 = 0, psum5 = 0, psum6 = 0, psum7 = 0;
        for (int col = 0; col < multiple; col += 8) {
          int off = idx + col;
          psum0 += a[off    ] * x[col    ];
          psum1 += a[off + 1] * x[col + 1];
          psum2 += a[off + 2] * x[col + 2];
          psum3 += a[off + 3] * x[col + 3];
          psum4 += a[off + 4] * x[col + 4];
          psum5 += a[off + 5] * x[col + 5];
          psum6 += a[off + 6] * x[col + 6];
          psum7 += a[off + 7] * x[col + 7];
        }
        res[row] += psum0 + psum1 + psum2 + psum3;
        res[row] += psum4 + psum5 + psum6 + psum7;
        for (int col = extra; col < cols; col++)
          res[row] += a[idx + col] * x[col];
        res[row] += y[row];
        idx += cols;
      }
      if (i<ACTIVATION.length-1) {
        for (int r=0; r<ACTIVATION[i].length; ++r) {
          ACTIVATION[i][r] = Math.max(0, ACTIVATION[i][r]);
        }
      }
      if (i == ACTIVATION.length-1) {
        double max = ACTIVATION[i][0];
        for (int r=1; r<ACTIVATION[i].length; r++) {
          if (ACTIVATION[i][r]>max) max = ACTIVATION[i][r];
        }
        double scale = 0;
        for (int r=0; r<ACTIVATION[i].length; r++) {
          ACTIVATION[i][r] = Math.exp(ACTIVATION[i][r] - max);
          scale += ACTIVATION[i][r];
        }
        for (int r=0; r<ACTIVATION[i].length; r++) {
          if (Double.isNaN(ACTIVATION[i][r]))
            throw new RuntimeException("Numerical instability, predicted NaN.");
          ACTIVATION[i][r] /= scale;
          preds[r+1] = ACTIVATION[i][r];
        }
      }
    }
    preds[0] = hex.genmodel.GenModel.getPrediction(preds, PRIOR_CLASS_DISTRIB, data, 0.5);
    return preds;
  }
}
// Neuron activation values for Input layer
class h2o_nn_16x16x6_ReLU_02_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_16x16x6_ReLU_02_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_Activation_0_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
      sa[5] = 0.0;
      sa[6] = 0.0;
      sa[7] = 0.0;
      sa[8] = 0.0;
      sa[9] = 0.0;
      sa[10] = 0.0;
      sa[11] = 0.0;
      sa[12] = 0.0;
    }
  }
}
// Neuron activation values for Rectifier layer
class h2o_nn_16x16x6_ReLU_02_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_ReLU_02_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_Activation_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
      sa[5] = 0.0;
      sa[6] = 0.0;
      sa[7] = 0.0;
      sa[8] = 0.0;
      sa[9] = 0.0;
      sa[10] = 0.0;
      sa[11] = 0.0;
      sa[12] = 0.0;
      sa[13] = 0.0;
      sa[14] = 0.0;
      sa[15] = 0.0;
    }
  }
}
// Neuron activation values for Rectifier layer
class h2o_nn_16x16x6_ReLU_02_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_ReLU_02_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_Activation_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
      sa[5] = 0.0;
      sa[6] = 0.0;
      sa[7] = 0.0;
      sa[8] = 0.0;
      sa[9] = 0.0;
      sa[10] = 0.0;
      sa[11] = 0.0;
      sa[12] = 0.0;
      sa[13] = 0.0;
      sa[14] = 0.0;
      sa[15] = 0.0;
    }
  }
}
// Neuron activation values for Softmax layer
class h2o_nn_16x16x6_ReLU_02_Activation_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_16x16x6_ReLU_02_Activation_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_Activation_3_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
    }
  }
}
// Neuron bias values for Input layer
class h2o_nn_16x16x6_ReLU_02_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Rectifier layer
class h2o_nn_16x16x6_ReLU_02_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_ReLU_02_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.6593475252314637;
      sa[1] = 0.6131856462005876;
      sa[2] = 0.6224977586658743;
      sa[3] = 0.5170449356258893;
      sa[4] = 0.46415130630964696;
      sa[5] = 0.6430359202647786;
      sa[6] = 0.3340300687560658;
      sa[7] = 0.5853000814175331;
      sa[8] = 0.6152388675953389;
      sa[9] = 0.5485198085876567;
      sa[10] = 0.5252103031523835;
      sa[11] = 0.5077822578005255;
      sa[12] = 0.30253861724555864;
      sa[13] = 0.5927882989995128;
      sa[14] = 0.5454105946794825;
      sa[15] = 0.5117829494830345;
    }
  }
}
// Neuron bias values for Rectifier layer
class h2o_nn_16x16x6_ReLU_02_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_ReLU_02_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 1.0775111839683873;
      sa[1] = 1.0203055520714437;
      sa[2] = 0.9097345360978856;
      sa[3] = 0.9664892942707971;
      sa[4] = 0.962902961495444;
      sa[5] = 0.9872540635921865;
      sa[6] = 0.9712707290951074;
      sa[7] = 0.8990103309986032;
      sa[8] = 0.8596994639121188;
      sa[9] = 1.0825731414846016;
      sa[10] = 0.893507643838476;
      sa[11] = 1.019994363288273;
      sa[12] = 0.9979586843742199;
      sa[13] = 1.0386149045401667;
      sa[14] = 1.1115750852868025;
      sa[15] = 1.0275532016857387;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_16x16x6_ReLU_02_Bias_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_16x16x6_ReLU_02_Bias_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_Bias_3_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = -0.07996189468771571;
      sa[1] = 0.017611814855554363;
      sa[2] = -0.044009141097954715;
      sa[3] = 0.004658903207007358;
      sa[4] = 0.054625113981342226;
    }
  }
}
class h2o_nn_16x16x6_ReLU_02_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Rectifier layer
class h2o_nn_16x16x6_ReLU_02_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[208];
  static {
    h2o_nn_16x16x6_ReLU_02_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.17476489f;
      sa[1] = 0.25809464f;
      sa[2] = 0.25246114f;
      sa[3] = 0.3212338f;
      sa[4] = 0.22820522f;
      sa[5] = -0.2610541f;
      sa[6] = -0.27899927f;
      sa[7] = 0.3893376f;
      sa[8] = 0.1620651f;
      sa[9] = -0.18051735f;
      sa[10] = 0.10653295f;
      sa[11] = -0.39840615f;
      sa[12] = 0.008491336f;
      sa[13] = -0.37568983f;
      sa[14] = -0.34611386f;
      sa[15] = -4.8478018E-4f;
      sa[16] = 0.24837989f;
      sa[17] = 0.2283624f;
      sa[18] = -0.36208674f;
      sa[19] = 0.17415383f;
      sa[20] = 0.35168937f;
      sa[21] = -0.5025199f;
      sa[22] = 0.38389996f;
      sa[23] = 0.093358286f;
      sa[24] = 0.26031625f;
      sa[25] = 0.15676907f;
      sa[26] = -0.28180796f;
      sa[27] = 0.43843803f;
      sa[28] = 0.4351197f;
      sa[29] = -0.49360126f;
      sa[30] = -0.6283661f;
      sa[31] = 0.33602715f;
      sa[32] = 0.39474723f;
      sa[33] = -0.39906478f;
      sa[34] = 0.3217209f;
      sa[35] = -0.04385341f;
      sa[36] = -0.17562987f;
      sa[37] = 0.49226806f;
      sa[38] = -0.1184834f;
      sa[39] = 0.3755947f;
      sa[40] = 0.06590515f;
      sa[41] = -0.07960107f;
      sa[42] = -0.16829903f;
      sa[43] = 0.47520506f;
      sa[44] = 0.2995399f;
      sa[45] = -0.24271321f;
      sa[46] = 0.04408325f;
      sa[47] = 0.05543987f;
      sa[48] = 0.32828662f;
      sa[49] = 0.5146361f;
      sa[50] = 0.1790875f;
      sa[51] = -0.34735018f;
      sa[52] = -0.044856466f;
      sa[53] = 0.4821969f;
      sa[54] = 0.32561544f;
      sa[55] = -0.029864335f;
      sa[56] = -0.097398214f;
      sa[57] = -0.40075383f;
      sa[58] = 0.4182369f;
      sa[59] = -0.29122284f;
      sa[60] = -0.39625993f;
      sa[61] = 0.28046992f;
      sa[62] = -0.042079918f;
      sa[63] = 0.10971432f;
      sa[64] = 0.09802072f;
      sa[65] = 0.015979094f;
      sa[66] = 0.07651834f;
      sa[67] = 0.25341067f;
      sa[68] = -0.1458526f;
      sa[69] = 0.29196396f;
      sa[70] = -0.10547832f;
      sa[71] = 0.31669292f;
      sa[72] = -0.19320373f;
      sa[73] = 0.26580328f;
      sa[74] = -0.12051597f;
      sa[75] = 0.047243033f;
      sa[76] = -0.38014618f;
      sa[77] = -0.114760466f;
      sa[78] = -0.253334f;
      sa[79] = 0.20774963f;
      sa[80] = -0.43607444f;
      sa[81] = -0.22420095f;
      sa[82] = 0.024620337f;
      sa[83] = -0.0055609364f;
      sa[84] = 0.06285317f;
      sa[85] = -0.1953701f;
      sa[86] = 0.23233755f;
      sa[87] = 0.16536038f;
      sa[88] = 0.2044967f;
      sa[89] = 0.06354533f;
      sa[90] = 0.026606208f;
      sa[91] = 0.21023479f;
      sa[92] = -0.020223798f;
      sa[93] = -0.4118957f;
      sa[94] = -0.3273748f;
      sa[95] = -0.021640142f;
      sa[96] = 0.07266946f;
      sa[97] = 0.17575829f;
      sa[98] = 0.46894184f;
      sa[99] = -0.063460715f;
      sa[100] = -0.14042929f;
      sa[101] = -0.0833076f;
      sa[102] = -0.601364f;
      sa[103] = 0.0059373295f;
      sa[104] = 0.066632874f;
      sa[105] = -0.02911042f;
      sa[106] = -0.28027466f;
      sa[107] = 0.36303955f;
      sa[108] = 0.18286918f;
      sa[109] = -0.15522146f;
      sa[110] = -0.675172f;
      sa[111] = 0.085154526f;
      sa[112] = 0.022383815f;
      sa[113] = 0.21843414f;
      sa[114] = 0.14652881f;
      sa[115] = 0.05713793f;
      sa[116] = -0.61494493f;
      sa[117] = 0.026483428f;
      sa[118] = -0.47689798f;
      sa[119] = -0.35144797f;
      sa[120] = 0.34479743f;
      sa[121] = 0.62836605f;
      sa[122] = 0.22791596f;
      sa[123] = 0.47321457f;
      sa[124] = 0.4497611f;
      sa[125] = 0.4115297f;
      sa[126] = -0.49014115f;
      sa[127] = -0.035487674f;
      sa[128] = -0.034189373f;
      sa[129] = 0.16813433f;
      sa[130] = -0.0675703f;
      sa[131] = 0.56502485f;
      sa[132] = 0.30015558f;
      sa[133] = -0.5263416f;
      sa[134] = 0.37260008f;
      sa[135] = -0.3697962f;
      sa[136] = -0.2443044f;
      sa[137] = 0.008558446f;
      sa[138] = 0.35345972f;
      sa[139] = -0.16079074f;
      sa[140] = -0.39869565f;
      sa[141] = -0.08562853f;
      sa[142] = 0.39588252f;
      sa[143] = -0.4831111f;
      sa[144] = 0.32861152f;
      sa[145] = -0.2088743f;
      sa[146] = -0.42577785f;
      sa[147] = 0.14542188f;
      sa[148] = 0.1454225f;
      sa[149] = 0.42596704f;
      sa[150] = 0.51474696f;
      sa[151] = -0.27287665f;
      sa[152] = -0.19584563f;
      sa[153] = 0.23437962f;
      sa[154] = 0.29192013f;
      sa[155] = -0.11927879f;
      sa[156] = 0.14560387f;
      sa[157] = -0.36805254f;
      sa[158] = 0.03374568f;
      sa[159] = -0.05903246f;
      sa[160] = 0.14702941f;
      sa[161] = 0.16768582f;
      sa[162] = 0.06523033f;
      sa[163] = -0.04490856f;
      sa[164] = 0.39419287f;
      sa[165] = -0.17410669f;
      sa[166] = -0.19221151f;
      sa[167] = -0.24701187f;
      sa[168] = 0.47811794f;
      sa[169] = 0.5954451f;
      sa[170] = 0.073890984f;
      sa[171] = 0.018072583f;
      sa[172] = 0.08525571f;
      sa[173] = -0.11109401f;
      sa[174] = 0.049056083f;
      sa[175] = -0.3565854f;
      sa[176] = -0.16678415f;
      sa[177] = -0.120205805f;
      sa[178] = 0.11205405f;
      sa[179] = 0.3551005f;
      sa[180] = 0.05701072f;
      sa[181] = -0.5888353f;
      sa[182] = -0.2615209f;
      sa[183] = 0.055569287f;
      sa[184] = 0.414379f;
      sa[185] = 0.2292783f;
      sa[186] = -0.24112743f;
      sa[187] = 0.09206185f;
      sa[188] = -0.1437566f;
      sa[189] = 0.36765036f;
      sa[190] = 0.08945392f;
      sa[191] = -0.08829376f;
      sa[192] = -0.22406012f;
      sa[193] = 0.24885456f;
      sa[194] = 0.16973028f;
      sa[195] = 0.5443368f;
      sa[196] = 0.39445525f;
      sa[197] = -0.066916525f;
      sa[198] = 0.16178189f;
      sa[199] = 0.30955654f;
      sa[200] = -0.015112926f;
      sa[201] = -0.43707642f;
      sa[202] = 0.16368182f;
      sa[203] = -0.07521351f;
      sa[204] = -0.17051399f;
      sa[205] = 0.5459449f;
      sa[206] = 0.30417135f;
      sa[207] = 0.094495535f;
    }
  }
}
// Neuron weights connecting Rectifier and Rectifier layer
class h2o_nn_16x16x6_ReLU_02_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[256];
  static {
    h2o_nn_16x16x6_ReLU_02_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.40400204f;
      sa[1] = 0.04574637f;
      sa[2] = -0.26892534f;
      sa[3] = -0.24670017f;
      sa[4] = 6.575607E-4f;
      sa[5] = 0.13951804f;
      sa[6] = 0.34589165f;
      sa[7] = -0.062655225f;
      sa[8] = -0.2600421f;
      sa[9] = 0.29288504f;
      sa[10] = -0.16238502f;
      sa[11] = 0.108124666f;
      sa[12] = 0.16263269f;
      sa[13] = 0.038172055f;
      sa[14] = -0.35482496f;
      sa[15] = 0.29801306f;
      sa[16] = 0.46961024f;
      sa[17] = -0.046302732f;
      sa[18] = -0.109118216f;
      sa[19] = -0.20563102f;
      sa[20] = 0.2967748f;
      sa[21] = 0.0711449f;
      sa[22] = -0.3409952f;
      sa[23] = 0.30557704f;
      sa[24] = -0.42790765f;
      sa[25] = 0.27841458f;
      sa[26] = -0.021336054f;
      sa[27] = 0.4101155f;
      sa[28] = -0.28529787f;
      sa[29] = -0.23387535f;
      sa[30] = 0.0959654f;
      sa[31] = -0.24670172f;
      sa[32] = -0.3232915f;
      sa[33] = -0.41167802f;
      sa[34] = -0.14262246f;
      sa[35] = 0.25972602f;
      sa[36] = -0.15775426f;
      sa[37] = -0.022327926f;
      sa[38] = -0.0643216f;
      sa[39] = 0.24814865f;
      sa[40] = -0.35186696f;
      sa[41] = -0.3091933f;
      sa[42] = -0.37225476f;
      sa[43] = -0.48540995f;
      sa[44] = 0.06320567f;
      sa[45] = -0.50294524f;
      sa[46] = 0.27665266f;
      sa[47] = 0.27812275f;
      sa[48] = -0.40493366f;
      sa[49] = -0.33316195f;
      sa[50] = 0.18396023f;
      sa[51] = -0.16636185f;
      sa[52] = 0.35318252f;
      sa[53] = 0.22728932f;
      sa[54] = -0.1007789f;
      sa[55] = 0.40577897f;
      sa[56] = -0.3640397f;
      sa[57] = -0.27347082f;
      sa[58] = 0.048709903f;
      sa[59] = -0.06824327f;
      sa[60] = 0.37394008f;
      sa[61] = -0.24215244f;
      sa[62] = -0.06193201f;
      sa[63] = 0.05715726f;
      sa[64] = 0.3634457f;
      sa[65] = -0.15052378f;
      sa[66] = 0.2032077f;
      sa[67] = 0.4110697f;
      sa[68] = 0.23143816f;
      sa[69] = -0.21769731f;
      sa[70] = -0.4119791f;
      sa[71] = -0.12411385f;
      sa[72] = -0.43316504f;
      sa[73] = 0.34308022f;
      sa[74] = 0.41904214f;
      sa[75] = 0.038888846f;
      sa[76] = 0.22412324f;
      sa[77] = 0.07382416f;
      sa[78] = -0.4000695f;
      sa[79] = 0.23895587f;
      sa[80] = -0.25217375f;
      sa[81] = 0.05256776f;
      sa[82] = 0.22652653f;
      sa[83] = -0.19098812f;
      sa[84] = -0.51781243f;
      sa[85] = -0.4186573f;
      sa[86] = 0.043719888f;
      sa[87] = 0.3314447f;
      sa[88] = -0.39994735f;
      sa[89] = -0.17265189f;
      sa[90] = -0.3437283f;
      sa[91] = 0.35387927f;
      sa[92] = -0.086177245f;
      sa[93] = 0.083556764f;
      sa[94] = -0.33148363f;
      sa[95] = -0.13092774f;
      sa[96] = -0.2988473f;
      sa[97] = 0.08057014f;
      sa[98] = -0.108659156f;
      sa[99] = -0.17076467f;
      sa[100] = -0.32183728f;
      sa[101] = -0.07264913f;
      sa[102] = 0.21511622f;
      sa[103] = 0.11894111f;
      sa[104] = -0.23867553f;
      sa[105] = -0.05587006f;
      sa[106] = 0.35333067f;
      sa[107] = 0.2221624f;
      sa[108] = 0.28533006f;
      sa[109] = -0.18167563f;
      sa[110] = 0.18187942f;
      sa[111] = -0.3864074f;
      sa[112] = -0.39663374f;
      sa[113] = -0.11732107f;
      sa[114] = 0.20366716f;
      sa[115] = -0.16406693f;
      sa[116] = 0.38547048f;
      sa[117] = -0.08479401f;
      sa[118] = -0.41456765f;
      sa[119] = -0.25583604f;
      sa[120] = -0.16715188f;
      sa[121] = 0.18756545f;
      sa[122] = -0.06138402f;
      sa[123] = 0.042815316f;
      sa[124] = -0.26532787f;
      sa[125] = 0.15179023f;
      sa[126] = -0.21287666f;
      sa[127] = -0.016515002f;
      sa[128] = -0.2974134f;
      sa[129] = 0.09643471f;
      sa[130] = 0.16391532f;
      sa[131] = 0.14265503f;
      sa[132] = 0.18982498f;
      sa[133] = 0.06528426f;
      sa[134] = 0.11017426f;
      sa[135] = -0.43347454f;
      sa[136] = 0.04341761f;
      sa[137] = -0.2961458f;
      sa[138] = 0.08868809f;
      sa[139] = 0.29950458f;
      sa[140] = -0.24754837f;
      sa[141] = -0.12380014f;
      sa[142] = 0.13004698f;
      sa[143] = -0.30612683f;
      sa[144] = 0.32838115f;
      sa[145] = -0.23522113f;
      sa[146] = 0.07986082f;
      sa[147] = 0.01957333f;
      sa[148] = -0.3715235f;
      sa[149] = 0.14411008f;
      sa[150] = -0.23014504f;
      sa[151] = -0.4794595f;
      sa[152] = 0.5574378f;
      sa[153] = -0.4858885f;
      sa[154] = 0.3726634f;
      sa[155] = -0.44903043f;
      sa[156] = -0.40861052f;
      sa[157] = -0.1642691f;
      sa[158] = 0.020209115f;
      sa[159] = -0.06046724f;
      sa[160] = 0.28420326f;
      sa[161] = -0.42870718f;
      sa[162] = 0.61044294f;
      sa[163] = -0.29743892f;
      sa[164] = 0.20626903f;
      sa[165] = -0.22880927f;
      sa[166] = 0.27054277f;
      sa[167] = -0.4017586f;
      sa[168] = 0.16671512f;
      sa[169] = -0.49513376f;
      sa[170] = 0.3455522f;
      sa[171] = 0.17598109f;
      sa[172] = 0.3407747f;
      sa[173] = 0.32838312f;
      sa[174] = 0.06596119f;
      sa[175] = -0.15327041f;
      sa[176] = -0.15759797f;
      sa[177] = 0.051247735f;
      sa[178] = -0.31478187f;
      sa[179] = -0.020737756f;
      sa[180] = -0.3569406f;
      sa[181] = -0.19737601f;
      sa[182] = 0.14496638f;
      sa[183] = -0.25175682f;
      sa[184] = 0.36935315f;
      sa[185] = 0.3680114f;
      sa[186] = -0.16757692f;
      sa[187] = -0.21092571f;
      sa[188] = -0.13246411f;
      sa[189] = -0.15722553f;
      sa[190] = -0.4722614f;
      sa[191] = -0.22397411f;
      sa[192] = -0.056118805f;
      sa[193] = 0.09390643f;
      sa[194] = -0.22216448f;
      sa[195] = -0.09645414f;
      sa[196] = 0.15777881f;
      sa[197] = -0.22679298f;
      sa[198] = 0.0766797f;
      sa[199] = 0.34133846f;
      sa[200] = 0.054424133f;
      sa[201] = 0.18723729f;
      sa[202] = 0.24332368f;
      sa[203] = -0.3438718f;
      sa[204] = 0.0711785f;
      sa[205] = 0.46463558f;
      sa[206] = -0.15089585f;
      sa[207] = 0.26392874f;
      sa[208] = -0.18973757f;
      sa[209] = 0.21081711f;
      sa[210] = -0.106980406f;
      sa[211] = -0.26333904f;
      sa[212] = -0.35479665f;
      sa[213] = 0.057373103f;
      sa[214] = -0.3019192f;
      sa[215] = -0.009280582f;
      sa[216] = 0.12712836f;
      sa[217] = 0.4023922f;
      sa[218] = -0.057945978f;
      sa[219] = 0.07828808f;
      sa[220] = 0.19206496f;
      sa[221] = -0.0017149749f;
      sa[222] = -0.26987267f;
      sa[223] = -0.36590892f;
      sa[224] = 0.33216012f;
      sa[225] = 0.108268276f;
      sa[226] = -0.13122214f;
      sa[227] = 0.14860187f;
      sa[228] = -0.16901228f;
      sa[229] = -0.21273692f;
      sa[230] = -0.1751913f;
      sa[231] = -0.09038093f;
      sa[232] = 0.5839154f;
      sa[233] = -0.15697195f;
      sa[234] = -0.21500714f;
      sa[235] = 0.012103542f;
      sa[236] = -0.32287145f;
      sa[237] = 0.5326913f;
      sa[238] = 0.18073697f;
      sa[239] = 0.026130863f;
      sa[240] = -0.05589606f;
      sa[241] = -0.34462062f;
      sa[242] = -0.36149156f;
      sa[243] = 0.42002058f;
      sa[244] = 0.11300022f;
      sa[245] = 0.30289695f;
      sa[246] = 0.20124982f;
      sa[247] = -0.34173396f;
      sa[248] = -0.12894088f;
      sa[249] = -0.017731544f;
      sa[250] = 0.28740853f;
      sa[251] = 0.44716433f;
      sa[252] = 0.2086077f;
      sa[253] = -0.28269526f;
      sa[254] = 0.36971208f;
      sa[255] = 0.30750036f;
    }
  }
}
// Neuron weights connecting Rectifier and Softmax layer
class h2o_nn_16x16x6_ReLU_02_Weight_3 implements java.io.Serializable {
  public static final float[] VALUES = new float[80];
  static {
    h2o_nn_16x16x6_ReLU_02_Weight_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_Weight_3_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = -0.017035665f;
      sa[1] = -1.613028f;
      sa[2] = -1.5732228f;
      sa[3] = 1.3589408f;
      sa[4] = 0.35557356f;
      sa[5] = -0.2560407f;
      sa[6] = -1.702044f;
      sa[7] = 1.5220121f;
      sa[8] = 1.8148198f;
      sa[9] = 0.213466f;
      sa[10] = 1.6977931f;
      sa[11] = 1.3853204f;
      sa[12] = 1.7552294f;
      sa[13] = -0.7743868f;
      sa[14] = -2.0005014f;
      sa[15] = -0.7684079f;
      sa[16] = 0.7467891f;
      sa[17] = -0.92754775f;
      sa[18] = -1.5061806f;
      sa[19] = -1.0915568f;
      sa[20] = -1.9376073f;
      sa[21] = 0.21215539f;
      sa[22] = -2.094525f;
      sa[23] = -0.63211554f;
      sa[24] = -0.5342614f;
      sa[25] = 1.9505512f;
      sa[26] = -0.307821f;
      sa[27] = 1.1557409f;
      sa[28] = 1.2418793f;
      sa[29] = -0.87349665f;
      sa[30] = 0.23926543f;
      sa[31] = 0.14954478f;
      sa[32] = 0.45026717f;
      sa[33] = 0.30816713f;
      sa[34] = -0.6297464f;
      sa[35] = 1.0710763f;
      sa[36] = 0.55542123f;
      sa[37] = -1.3998066f;
      sa[38] = 0.5948357f;
      sa[39] = -0.23675229f;
      sa[40] = 0.13960904f;
      sa[41] = -0.8533059f;
      sa[42] = 0.45945033f;
      sa[43] = -0.061283376f;
      sa[44] = 0.98159724f;
      sa[45] = -1.9891504f;
      sa[46] = -2.260886f;
      sa[47] = 0.008182297f;
      sa[48] = 1.6578196f;
      sa[49] = -0.19859487f;
      sa[50] = -0.4942163f;
      sa[51] = 1.8226047f;
      sa[52] = -1.7305219f;
      sa[53] = 1.0439225f;
      sa[54] = 1.8190614f;
      sa[55] = 1.1071076f;
      sa[56] = -0.6056854f;
      sa[57] = -0.100953594f;
      sa[58] = 0.9950428f;
      sa[59] = 0.8899801f;
      sa[60] = -1.3834819f;
      sa[61] = 1.0158014f;
      sa[62] = -0.4641509f;
      sa[63] = -0.8537092f;
      sa[64] = 0.9968564f;
      sa[65] = -0.9489921f;
      sa[66] = -1.814308f;
      sa[67] = -0.16711125f;
      sa[68] = 0.9522434f;
      sa[69] = -1.3764286f;
      sa[70] = -1.678199f;
      sa[71] = -0.0524019f;
      sa[72] = -1.0529709f;
      sa[73] = -1.3671366f;
      sa[74] = -1.2427853f;
      sa[75] = 1.3406066f;
      sa[76] = 1.9479377f;
      sa[77] = -0.55577767f;
      sa[78] = -1.363025f;
      sa[79] = -0.32766566f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_16x16x6_ReLU_02 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_16x16x6_ReLU_02_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_16x16x6_ReLU_02_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_ReLU_02_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    h2o_nn_16x16x6_ReLU_02_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_02_ColInfo_13_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "4";
      sa[3] = "5";
      sa[4] = "6";
    }
  }
}


