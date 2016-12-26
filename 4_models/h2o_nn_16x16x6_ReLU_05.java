/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:41:56.643+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_16x16x6_ReLU_05

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_16x16x6_ReLU_05 > h2o_nn_16x16x6_ReLU_05.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_16x16x6_ReLU_05.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_16x16x6_ReLU_05", algorithm="deeplearning")
public class h2o_nn_16x16x6_ReLU_05 extends GenModel {
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
      /* Input */ h2o_nn_16x16x6_ReLU_05_Activation_0.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_05_Activation_1.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_05_Activation_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_ReLU_05_Activation_3.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_16x16x6_ReLU_05_Bias_0.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_05_Bias_1.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_05_Bias_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_ReLU_05_Bias_3.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_16x16x6_ReLU_05_Weight_0.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_05_Weight_1.VALUES,
      /* Rectifier */ h2o_nn_16x16x6_ReLU_05_Weight_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_ReLU_05_Weight_3.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_16x16x6_ReLU_05.VALUES;
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
    /* Label */ h2o_nn_16x16x6_ReLU_05_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.29347826086956524,0.13043478260869565,0.14130434782608695,0.29347826086956524,0.14130434782608695};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_16x16x6_ReLU_05() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-2988498255812263296L); }

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
class h2o_nn_16x16x6_ReLU_05_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_16x16x6_ReLU_05_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_Activation_0_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_ReLU_05_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_ReLU_05_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_Activation_1_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_ReLU_05_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_ReLU_05_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_Activation_2_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_ReLU_05_Activation_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_16x16x6_ReLU_05_Activation_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_Activation_3_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_ReLU_05_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Rectifier layer
class h2o_nn_16x16x6_ReLU_05_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_ReLU_05_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.6279647554759222;
      sa[1] = 0.6811433118104181;
      sa[2] = 0.5262174161344159;
      sa[3] = 0.3473327702440758;
      sa[4] = 0.4657652221563164;
      sa[5] = 0.533273320104186;
      sa[6] = 0.39425687545009597;
      sa[7] = 0.7589598308249318;
      sa[8] = 0.5772732459338032;
      sa[9] = 0.6143969811932486;
      sa[10] = 0.37363985911643416;
      sa[11] = 0.26089634279372714;
      sa[12] = 0.42105548561481665;
      sa[13] = 0.487622277103765;
      sa[14] = 0.5277054756447802;
      sa[15] = 0.4343373915995628;
    }
  }
}
// Neuron bias values for Rectifier layer
class h2o_nn_16x16x6_ReLU_05_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_ReLU_05_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 1.0517151258783837;
      sa[1] = 1.014901586707132;
      sa[2] = 0.9569900973059831;
      sa[3] = 0.9592106777619819;
      sa[4] = 0.9720142658443667;
      sa[5] = 1.008078939845502;
      sa[6] = 0.9438340978611589;
      sa[7] = 0.9111629223135053;
      sa[8] = 0.9042332762265647;
      sa[9] = 0.9936239941949807;
      sa[10] = 0.885534373910735;
      sa[11] = 1.1068569562243622;
      sa[12] = 1.0189667012799595;
      sa[13] = 1.006587276476216;
      sa[14] = 1.0951189449637333;
      sa[15] = 1.0769030479310173;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_16x16x6_ReLU_05_Bias_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_16x16x6_ReLU_05_Bias_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_Bias_3_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = -0.06917556492985505;
      sa[1] = 0.023557339884414018;
      sa[2] = -0.03834529812932497;
      sa[3] = -0.01117752112567407;
      sa[4] = 0.05196995087570057;
    }
  }
}
class h2o_nn_16x16x6_ReLU_05_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Rectifier layer
class h2o_nn_16x16x6_ReLU_05_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[208];
  static {
    h2o_nn_16x16x6_ReLU_05_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.19828697f;
      sa[1] = 0.25164956f;
      sa[2] = 0.37499171f;
      sa[3] = 0.39555812f;
      sa[4] = 0.020762686f;
      sa[5] = -0.29127863f;
      sa[6] = 0.008967597f;
      sa[7] = 0.45320874f;
      sa[8] = 0.2755558f;
      sa[9] = -0.25507393f;
      sa[10] = -0.1658156f;
      sa[11] = -0.34092104f;
      sa[12] = 0.11360563f;
      sa[13] = -0.15926497f;
      sa[14] = -0.45489317f;
      sa[15] = 0.127827f;
      sa[16] = 0.07970584f;
      sa[17] = 0.4664972f;
      sa[18] = -0.1403877f;
      sa[19] = 0.035487045f;
      sa[20] = 0.44946417f;
      sa[21] = -0.35759455f;
      sa[22] = 0.5000347f;
      sa[23] = -0.03128163f;
      sa[24] = 0.592264f;
      sa[25] = 0.19153604f;
      sa[26] = -0.49427408f;
      sa[27] = 0.44804496f;
      sa[28] = 0.2618983f;
      sa[29] = -0.47806683f;
      sa[30] = -0.4556559f;
      sa[31] = 0.23278904f;
      sa[32] = 0.26091003f;
      sa[33] = -0.4537348f;
      sa[34] = 0.107556656f;
      sa[35] = -0.10743552f;
      sa[36] = 0.12468032f;
      sa[37] = 0.32785082f;
      sa[38] = -0.094838925f;
      sa[39] = 0.21819247f;
      sa[40] = 0.0061400565f;
      sa[41] = -0.24700601f;
      sa[42] = -0.16384481f;
      sa[43] = 0.11682963f;
      sa[44] = 0.24414994f;
      sa[45] = -0.14210767f;
      sa[46] = -0.016640356f;
      sa[47] = -0.06731357f;
      sa[48] = 0.32111293f;
      sa[49] = 0.63861877f;
      sa[50] = 0.34430292f;
      sa[51] = -0.54519784f;
      sa[52] = -0.09833508f;
      sa[53] = 0.2838612f;
      sa[54] = 0.2708155f;
      sa[55] = 0.10411665f;
      sa[56] = -0.16083342f;
      sa[57] = -0.5440237f;
      sa[58] = 0.6460006f;
      sa[59] = -0.27316478f;
      sa[60] = -0.43106395f;
      sa[61] = 0.21877752f;
      sa[62] = 0.21370506f;
      sa[63] = -0.023504822f;
      sa[64] = 0.103204206f;
      sa[65] = 0.35621795f;
      sa[66] = -0.0075890897f;
      sa[67] = 0.48359504f;
      sa[68] = 0.25289506f;
      sa[69] = 0.7327664f;
      sa[70] = -0.49113575f;
      sa[71] = 0.6158977f;
      sa[72] = -0.13441171f;
      sa[73] = 0.40005776f;
      sa[74] = -0.34941337f;
      sa[75] = -0.047881257f;
      sa[76] = -0.44412205f;
      sa[77] = -0.28845853f;
      sa[78] = -0.1736276f;
      sa[79] = 0.35108048f;
      sa[80] = -0.34339476f;
      sa[81] = -0.2083484f;
      sa[82] = -0.14928596f;
      sa[83] = 0.17215778f;
      sa[84] = -0.050705742f;
      sa[85] = -0.34145185f;
      sa[86] = 0.18764284f;
      sa[87] = 0.3194997f;
      sa[88] = 0.21503238f;
      sa[89] = 0.011638978f;
      sa[90] = -0.049871486f;
      sa[91] = 0.39758903f;
      sa[92] = 0.027699431f;
      sa[93] = -0.10517311f;
      sa[94] = -0.16632482f;
      sa[95] = -0.019260384f;
      sa[96] = 0.18120007f;
      sa[97] = 0.23759033f;
      sa[98] = 0.47512862f;
      sa[99] = 0.067727946f;
      sa[100] = -0.048778772f;
      sa[101] = -0.16964655f;
      sa[102] = -0.6190463f;
      sa[103] = 0.15859231f;
      sa[104] = 0.10193009f;
      sa[105] = -0.16478631f;
      sa[106] = -0.28357035f;
      sa[107] = 0.09186134f;
      sa[108] = 0.35629162f;
      sa[109] = -0.13000728f;
      sa[110] = -0.40991613f;
      sa[111] = 0.26485667f;
      sa[112] = 0.22549711f;
      sa[113] = 0.13049574f;
      sa[114] = -0.23523988f;
      sa[115] = 0.19669047f;
      sa[116] = -0.55897117f;
      sa[117] = 0.11540802f;
      sa[118] = -0.5014361f;
      sa[119] = -0.2261393f;
      sa[120] = 0.3627026f;
      sa[121] = 0.59744537f;
      sa[122] = 0.4186985f;
      sa[123] = 0.49494666f;
      sa[124] = 0.50554705f;
      sa[125] = 0.44474113f;
      sa[126] = -0.3299005f;
      sa[127] = -0.028599167f;
      sa[128] = 0.16094665f;
      sa[129] = 0.18505089f;
      sa[130] = -0.26079306f;
      sa[131] = 0.45039508f;
      sa[132] = 0.17204711f;
      sa[133] = -0.33217746f;
      sa[134] = 0.18464169f;
      sa[135] = -0.46738806f;
      sa[136] = -0.19968572f;
      sa[137] = -0.06934649f;
      sa[138] = 0.12746376f;
      sa[139] = -0.23266697f;
      sa[140] = -0.1515471f;
      sa[141] = -0.26485443f;
      sa[142] = 0.2551421f;
      sa[143] = -0.51925236f;
      sa[144] = 0.4948455f;
      sa[145] = -0.29878533f;
      sa[146] = -0.35854432f;
      sa[147] = 0.2792702f;
      sa[148] = 0.17080922f;
      sa[149] = 0.28523114f;
      sa[150] = 0.32851392f;
      sa[151] = -0.46794298f;
      sa[152] = -0.006540102f;
      sa[153] = 0.4037776f;
      sa[154] = 0.33224544f;
      sa[155] = -0.19395329f;
      sa[156] = 0.111704186f;
      sa[157] = -0.2919634f;
      sa[158] = 0.06526183f;
      sa[159] = 0.1377809f;
      sa[160] = 0.008408914f;
      sa[161] = 0.2363027f;
      sa[162] = -0.14153336f;
      sa[163] = -0.15121196f;
      sa[164] = 0.29601434f;
      sa[165] = -0.068310425f;
      sa[166] = -0.044986043f;
      sa[167] = -0.23123792f;
      sa[168] = 0.31432736f;
      sa[169] = 0.44475442f;
      sa[170] = 0.022880556f;
      sa[171] = -0.14553063f;
      sa[172] = -0.12926403f;
      sa[173] = -0.1542383f;
      sa[174] = -0.025770806f;
      sa[175] = -0.27838546f;
      sa[176] = -0.091394104f;
      sa[177] = -0.14286722f;
      sa[178] = -0.011765604f;
      sa[179] = 0.43452504f;
      sa[180] = 0.109838374f;
      sa[181] = -0.5934191f;
      sa[182] = -0.09902271f;
      sa[183] = 0.15595202f;
      sa[184] = 0.6355923f;
      sa[185] = 0.40512666f;
      sa[186] = -0.26098648f;
      sa[187] = 0.050291955f;
      sa[188] = 0.009424167f;
      sa[189] = 0.3637483f;
      sa[190] = 0.19463454f;
      sa[191] = -0.14489844f;
      sa[192] = -0.41355237f;
      sa[193] = 0.20515712f;
      sa[194] = 0.17478491f;
      sa[195] = 0.39065146f;
      sa[196] = 0.30144295f;
      sa[197] = -0.0880562f;
      sa[198] = 0.20753251f;
      sa[199] = 0.07503204f;
      sa[200] = -0.0981802f;
      sa[201] = -0.18120115f;
      sa[202] = 0.21284135f;
      sa[203] = -0.050346345f;
      sa[204] = -0.36547124f;
      sa[205] = 0.6230515f;
      sa[206] = 0.3540758f;
      sa[207] = 0.17146596f;
    }
  }
}
// Neuron weights connecting Rectifier and Rectifier layer
class h2o_nn_16x16x6_ReLU_05_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[256];
  static {
    h2o_nn_16x16x6_ReLU_05_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.41188973f;
      sa[1] = 0.021789916f;
      sa[2] = -0.21031314f;
      sa[3] = -0.30781683f;
      sa[4] = 0.03283505f;
      sa[5] = 0.017536877f;
      sa[6] = 0.32461342f;
      sa[7] = -0.12463963f;
      sa[8] = -0.33626312f;
      sa[9] = 0.25883287f;
      sa[10] = -0.010500462f;
      sa[11] = 0.027548147f;
      sa[12] = 0.16152394f;
      sa[13] = -0.045155864f;
      sa[14] = -0.29072747f;
      sa[15] = 0.35163078f;
      sa[16] = 0.5527037f;
      sa[17] = -0.16810833f;
      sa[18] = -0.09070477f;
      sa[19] = -0.23414947f;
      sa[20] = 0.39428243f;
      sa[21] = 0.10446406f;
      sa[22] = -0.34588003f;
      sa[23] = 0.29443607f;
      sa[24] = -0.42088446f;
      sa[25] = 0.18499222f;
      sa[26] = 0.021198796f;
      sa[27] = 0.31998652f;
      sa[28] = -0.30422717f;
      sa[29] = -0.22392516f;
      sa[30] = 0.14377645f;
      sa[31] = -0.23886845f;
      sa[32] = -0.20615686f;
      sa[33] = -0.5107789f;
      sa[34] = -0.04834132f;
      sa[35] = 0.21968983f;
      sa[36] = -0.06827976f;
      sa[37] = -0.023865148f;
      sa[38] = -0.012777685f;
      sa[39] = 0.262436f;
      sa[40] = -0.34115788f;
      sa[41] = -0.41454732f;
      sa[42] = -0.18409917f;
      sa[43] = -0.48043358f;
      sa[44] = 0.03054889f;
      sa[45] = -0.46872455f;
      sa[46] = 0.3495207f;
      sa[47] = 0.27274108f;
      sa[48] = -0.3640248f;
      sa[49] = -0.35711405f;
      sa[50] = 0.11771524f;
      sa[51] = -0.1299273f;
      sa[52] = 0.14381777f;
      sa[53] = 0.13515298f;
      sa[54] = -0.07421094f;
      sa[55] = 0.3172343f;
      sa[56] = -0.30279016f;
      sa[57] = -0.37128615f;
      sa[58] = 0.07318747f;
      sa[59] = -0.09814868f;
      sa[60] = 0.33894968f;
      sa[61] = -0.18504077f;
      sa[62] = -0.10035202f;
      sa[63] = 0.08516025f;
      sa[64] = 0.3525489f;
      sa[65] = -0.15463641f;
      sa[66] = 0.13364007f;
      sa[67] = 0.4331575f;
      sa[68] = 0.2751572f;
      sa[69] = -0.12922046f;
      sa[70] = -0.4132893f;
      sa[71] = -0.14402162f;
      sa[72] = -0.35379782f;
      sa[73] = 0.29578787f;
      sa[74] = 0.31031072f;
      sa[75] = 0.031766884f;
      sa[76] = 0.18535367f;
      sa[77] = 0.1355755f;
      sa[78] = -0.42280596f;
      sa[79] = 0.25731623f;
      sa[80] = -0.20711929f;
      sa[81] = 0.040000275f;
      sa[82] = 0.31062177f;
      sa[83] = -0.17631118f;
      sa[84] = -0.52288496f;
      sa[85] = -0.5223655f;
      sa[86] = 0.09728628f;
      sa[87] = 0.3482779f;
      sa[88] = -0.45570698f;
      sa[89] = -0.17252506f;
      sa[90] = -0.23662233f;
      sa[91] = 0.3769719f;
      sa[92] = -0.05053748f;
      sa[93] = 0.06877312f;
      sa[94] = -0.33938533f;
      sa[95] = -0.055208888f;
      sa[96] = -0.23315507f;
      sa[97] = -0.057785615f;
      sa[98] = -0.10407986f;
      sa[99] = -0.06249632f;
      sa[100] = -0.32146364f;
      sa[101] = -0.13794757f;
      sa[102] = 0.30108932f;
      sa[103] = 0.030813517f;
      sa[104] = -0.20627707f;
      sa[105] = -0.19591111f;
      sa[106] = 0.4696316f;
      sa[107] = 0.20086189f;
      sa[108] = 0.2291815f;
      sa[109] = -0.100435495f;
      sa[110] = 0.17235583f;
      sa[111] = -0.27983198f;
      sa[112] = -0.3872685f;
      sa[113] = -0.09211404f;
      sa[114] = 0.096109316f;
      sa[115] = -0.08572472f;
      sa[116] = 0.24152803f;
      sa[117] = -0.13654277f;
      sa[118] = -0.33530465f;
      sa[119] = -0.26242563f;
      sa[120] = -0.11498486f;
      sa[121] = 0.15206434f;
      sa[122] = -0.041515507f;
      sa[123] = 0.10681297f;
      sa[124] = -0.28635228f;
      sa[125] = 0.2017055f;
      sa[126] = -0.30150458f;
      sa[127] = 0.08213004f;
      sa[128] = -0.3468575f;
      sa[129] = 0.09386881f;
      sa[130] = -0.012856896f;
      sa[131] = 0.27792978f;
      sa[132] = 0.032851398f;
      sa[133] = 0.052891202f;
      sa[134] = 0.18806466f;
      sa[135] = -0.3855079f;
      sa[136] = 0.15808965f;
      sa[137] = -0.29550946f;
      sa[138] = -0.06705347f;
      sa[139] = 0.39995655f;
      sa[140] = -0.2550985f;
      sa[141] = 0.02343768f;
      sa[142] = 0.024531892f;
      sa[143] = -0.28657606f;
      sa[144] = 0.26802492f;
      sa[145] = -0.21792407f;
      sa[146] = 0.05566607f;
      sa[147] = -0.04292962f;
      sa[148] = -0.31655645f;
      sa[149] = 0.20459048f;
      sa[150] = -0.31558225f;
      sa[151] = -0.4547407f;
      sa[152] = 0.479327f;
      sa[153] = -0.18122211f;
      sa[154] = 0.37483007f;
      sa[155] = -0.42721012f;
      sa[156] = -0.39662358f;
      sa[157] = -0.25567922f;
      sa[158] = -0.013084293f;
      sa[159] = -0.16076891f;
      sa[160] = 0.2953471f;
      sa[161] = -0.41438517f;
      sa[162] = 0.49578214f;
      sa[163] = -0.23832142f;
      sa[164] = 0.1126151f;
      sa[165] = -0.26577812f;
      sa[166] = 0.35193318f;
      sa[167] = -0.42710844f;
      sa[168] = 0.24004361f;
      sa[169] = -0.530345f;
      sa[170] = 0.32000887f;
      sa[171] = 0.25585753f;
      sa[172] = 0.31526616f;
      sa[173] = 0.37855515f;
      sa[174] = -0.05521247f;
      sa[175] = -0.09397551f;
      sa[176] = -0.2651822f;
      sa[177] = 0.19047837f;
      sa[178] = -0.12861155f;
      sa[179] = 0.11796334f;
      sa[180] = -0.42251733f;
      sa[181] = -0.33916983f;
      sa[182] = 0.22456707f;
      sa[183] = -0.26536563f;
      sa[184] = 0.5037885f;
      sa[185] = 0.38214928f;
      sa[186] = -0.19172832f;
      sa[187] = -0.16638511f;
      sa[188] = -0.10165826f;
      sa[189] = -0.024323404f;
      sa[190] = -0.35900816f;
      sa[191] = -0.048425797f;
      sa[192] = -0.09436788f;
      sa[193] = 0.16929474f;
      sa[194] = -0.2445707f;
      sa[195] = -0.13527502f;
      sa[196] = 0.18275452f;
      sa[197] = -0.12067f;
      sa[198] = 0.031639352f;
      sa[199] = 0.3935849f;
      sa[200] = 0.08939781f;
      sa[201] = 0.2617006f;
      sa[202] = 0.11102087f;
      sa[203] = -0.30129796f;
      sa[204] = 0.08936608f;
      sa[205] = 0.46452874f;
      sa[206] = -0.15830927f;
      sa[207] = 0.19126956f;
      sa[208] = -0.22058676f;
      sa[209] = 0.18511309f;
      sa[210] = -0.10123799f;
      sa[211] = -0.31002635f;
      sa[212] = -0.4317881f;
      sa[213] = -0.15579236f;
      sa[214] = -0.2852612f;
      sa[215] = -0.04623754f;
      sa[216] = 0.013998726f;
      sa[217] = 0.34282526f;
      sa[218] = 0.058158018f;
      sa[219] = 0.0074632834f;
      sa[220] = 0.21041854f;
      sa[221] = -0.09183964f;
      sa[222] = -0.27350822f;
      sa[223] = -0.28222564f;
      sa[224] = 0.32151124f;
      sa[225] = 0.14058633f;
      sa[226] = -0.017818524f;
      sa[227] = 0.08918791f;
      sa[228] = -0.16633828f;
      sa[229] = -0.29583237f;
      sa[230] = -0.20907687f;
      sa[231] = -0.07761534f;
      sa[232] = 0.45979798f;
      sa[233] = -0.076169945f;
      sa[234] = -0.118626915f;
      sa[235] = 0.011024251f;
      sa[236] = -0.28073004f;
      sa[237] = 0.44395545f;
      sa[238] = 0.25389838f;
      sa[239] = -0.026509007f;
      sa[240] = -0.011363536f;
      sa[241] = -0.2910027f;
      sa[242] = -0.2718855f;
      sa[243] = 0.39346987f;
      sa[244] = 0.28541228f;
      sa[245] = 0.5237353f;
      sa[246] = 0.15441118f;
      sa[247] = -0.29012358f;
      sa[248] = -0.13577808f;
      sa[249] = 0.117659606f;
      sa[250] = 0.2354124f;
      sa[251] = 0.4553387f;
      sa[252] = 0.23134851f;
      sa[253] = -0.29118693f;
      sa[254] = 0.46331272f;
      sa[255] = 0.19906604f;
    }
  }
}
// Neuron weights connecting Rectifier and Softmax layer
class h2o_nn_16x16x6_ReLU_05_Weight_3 implements java.io.Serializable {
  public static final float[] VALUES = new float[80];
  static {
    h2o_nn_16x16x6_ReLU_05_Weight_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_Weight_3_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.020124666f;
      sa[1] = -1.6582867f;
      sa[2] = -1.6333125f;
      sa[3] = 1.2550064f;
      sa[4] = 0.38375345f;
      sa[5] = -0.1588961f;
      sa[6] = -1.6769738f;
      sa[7] = 1.513379f;
      sa[8] = 1.8562453f;
      sa[9] = 0.1529666f;
      sa[10] = 1.6725712f;
      sa[11] = 1.4308783f;
      sa[12] = 1.8171383f;
      sa[13] = -0.73998964f;
      sa[14] = -1.9411802f;
      sa[15] = -0.73902816f;
      sa[16] = 0.75583977f;
      sa[17] = -0.7683737f;
      sa[18] = -1.5271864f;
      sa[19] = -0.9090055f;
      sa[20] = -1.9268093f;
      sa[21] = 0.3055776f;
      sa[22] = -1.9826934f;
      sa[23] = -0.5976237f;
      sa[24] = -0.5415743f;
      sa[25] = 1.9013226f;
      sa[26] = -0.3279287f;
      sa[27] = 1.271306f;
      sa[28] = 1.2388554f;
      sa[29] = -0.717852f;
      sa[30] = 0.20774682f;
      sa[31] = 0.15479754f;
      sa[32] = 0.4723987f;
      sa[33] = 0.34508657f;
      sa[34] = -0.6856647f;
      sa[35] = 1.0584229f;
      sa[36] = 0.5352852f;
      sa[37] = -1.9057331f;
      sa[38] = 0.49290895f;
      sa[39] = -0.22420481f;
      sa[40] = 0.105791114f;
      sa[41] = -0.6723658f;
      sa[42] = 0.45192978f;
      sa[43] = -0.3154613f;
      sa[44] = 0.99244666f;
      sa[45] = -2.0475686f;
      sa[46] = -2.2227728f;
      sa[47] = 0.018867848f;
      sa[48] = 1.6561126f;
      sa[49] = -0.24815202f;
      sa[50] = -0.44565877f;
      sa[51] = 1.825366f;
      sa[52] = -1.7179983f;
      sa[53] = 0.9999536f;
      sa[54] = 1.8335267f;
      sa[55] = 1.0515355f;
      sa[56] = -0.62883955f;
      sa[57] = -0.09511437f;
      sa[58] = 1.0583881f;
      sa[59] = 0.6331104f;
      sa[60] = -1.3967556f;
      sa[61] = 0.96129185f;
      sa[62] = -0.47742873f;
      sa[63] = -0.85227287f;
      sa[64] = 0.9718734f;
      sa[65] = -0.9519663f;
      sa[66] = -1.7661211f;
      sa[67] = -0.12929724f;
      sa[68] = 0.9227696f;
      sa[69] = -1.32942f;
      sa[70] = -1.653112f;
      sa[71] = -0.015835f;
      sa[72] = -0.98808336f;
      sa[73] = -1.3063613f;
      sa[74] = -1.155884f;
      sa[75] = 1.3372065f;
      sa[76] = 1.9262371f;
      sa[77] = -0.5601533f;
      sa[78] = -1.4073557f;
      sa[79] = -0.34122017f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_16x16x6_ReLU_05 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_16x16x6_ReLU_05_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_16x16x6_ReLU_05_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_ReLU_05_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    h2o_nn_16x16x6_ReLU_05_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_ReLU_05_ColInfo_13_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "4";
      sa[3] = "5";
      sa[4] = "6";
    }
  }
}


