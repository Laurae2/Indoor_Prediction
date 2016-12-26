/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:43:40.564+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_16x16x6_Tanh_12

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_16x16x6_Tanh_12 > h2o_nn_16x16x6_Tanh_12.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_16x16x6_Tanh_12.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_16x16x6_Tanh_12", algorithm="deeplearning")
public class h2o_nn_16x16x6_Tanh_12 extends GenModel {
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
      /* Input */ h2o_nn_16x16x6_Tanh_12_Activation_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_12_Activation_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_12_Activation_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_12_Activation_3.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_16x16x6_Tanh_12_Bias_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_12_Bias_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_12_Bias_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_12_Bias_3.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_16x16x6_Tanh_12_Weight_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_12_Weight_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_12_Weight_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_12_Weight_3.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_16x16x6_Tanh_12.VALUES;
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
    /* Label */ h2o_nn_16x16x6_Tanh_12_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.2864864864864865,0.13513513513513514,0.14594594594594595,0.2864864864864865,0.14594594594594595};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_16x16x6_Tanh_12() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(6284232123756475520L); }

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
          ACTIVATION[i][r] = 1 - 2 / (1 + Math.exp(2*ACTIVATION[i][r]));
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
class h2o_nn_16x16x6_Tanh_12_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_16x16x6_Tanh_12_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_Activation_0_0 implements java.io.Serializable {
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
// Neuron activation values for Tanh layer
class h2o_nn_16x16x6_Tanh_12_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_12_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_Activation_1_0 implements java.io.Serializable {
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
// Neuron activation values for Tanh layer
class h2o_nn_16x16x6_Tanh_12_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_12_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_Activation_2_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_12_Activation_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_16x16x6_Tanh_12_Activation_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_Activation_3_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_12_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Tanh layer
class h2o_nn_16x16x6_Tanh_12_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_12_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = -0.010963111460545058;
      sa[1] = 0.01750734319060545;
      sa[2] = 0.049615637147547534;
      sa[3] = -0.025559521682910667;
      sa[4] = 0.09914669606694088;
      sa[5] = -0.03098037801740498;
      sa[6] = 0.05290397813207146;
      sa[7] = -0.08552124747459441;
      sa[8] = -0.01527158444815966;
      sa[9] = -0.025987012192284397;
      sa[10] = -0.004243208503580076;
      sa[11] = -0.055684057807348905;
      sa[12] = 0.004341459686857702;
      sa[13] = 0.048397046900814214;
      sa[14] = -0.030316075310798128;
      sa[15] = 0.024703827004018086;
    }
  }
}
// Neuron bias values for Tanh layer
class h2o_nn_16x16x6_Tanh_12_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_12_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.02647925934645087;
      sa[1] = -0.07029327933294567;
      sa[2] = -0.06449582249750603;
      sa[3] = 0.008767788828509905;
      sa[4] = 0.023633654932652695;
      sa[5] = -0.006959243260434796;
      sa[6] = -0.055244030749214336;
      sa[7] = -0.0028313141777351765;
      sa[8] = 0.009902200921640604;
      sa[9] = -0.015677846114291873;
      sa[10] = 0.09287972226148698;
      sa[11] = 0.03444486275755217;
      sa[12] = 0.11875265125229535;
      sa[13] = -0.02861754483006904;
      sa[14] = -0.06345905873728389;
      sa[15] = 0.06580393216779616;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_16x16x6_Tanh_12_Bias_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_16x16x6_Tanh_12_Bias_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_Bias_3_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.052510087443297805;
      sa[1] = -0.01743116491831377;
      sa[2] = -0.007276592517960081;
      sa[3] = -0.10648594522220087;
      sa[4] = -0.05502683706378229;
    }
  }
}
class h2o_nn_16x16x6_Tanh_12_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Tanh layer
class h2o_nn_16x16x6_Tanh_12_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[208];
  static {
    h2o_nn_16x16x6_Tanh_12_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.13158785f;
      sa[1] = 0.38984755f;
      sa[2] = 0.32537273f;
      sa[3] = 0.32552618f;
      sa[4] = -0.12583832f;
      sa[5] = -0.33884805f;
      sa[6] = -0.038277026f;
      sa[7] = 0.39287943f;
      sa[8] = 0.2105544f;
      sa[9] = -0.29083666f;
      sa[10] = -0.16861236f;
      sa[11] = -0.31368977f;
      sa[12] = 0.10000756f;
      sa[13] = -0.29678777f;
      sa[14] = -0.3009505f;
      sa[15] = -0.012941582f;
      sa[16] = 0.08070663f;
      sa[17] = 0.2560308f;
      sa[18] = -0.30595556f;
      sa[19] = 0.17570585f;
      sa[20] = 0.36880475f;
      sa[21] = -0.4211831f;
      sa[22] = 0.39050865f;
      sa[23] = -0.09549397f;
      sa[24] = 0.48499155f;
      sa[25] = 0.24810404f;
      sa[26] = -0.52656513f;
      sa[27] = 0.45803925f;
      sa[28] = 0.31928465f;
      sa[29] = -0.47963092f;
      sa[30] = -0.43867698f;
      sa[31] = 0.29837123f;
      sa[32] = 0.263767f;
      sa[33] = -0.4666483f;
      sa[34] = 0.11827109f;
      sa[35] = -0.08613933f;
      sa[36] = -0.058926135f;
      sa[37] = 0.38460824f;
      sa[38] = -0.20863387f;
      sa[39] = 0.25166893f;
      sa[40] = -0.0033982962f;
      sa[41] = -0.11663276f;
      sa[42] = -0.15085934f;
      sa[43] = 0.113710426f;
      sa[44] = 0.20917569f;
      sa[45] = -0.17014281f;
      sa[46] = 0.02462061f;
      sa[47] = -0.0032355653f;
      sa[48] = 0.27846196f;
      sa[49] = 0.53690505f;
      sa[50] = 0.31628418f;
      sa[51] = -0.53410053f;
      sa[52] = -0.14945191f;
      sa[53] = 0.25937006f;
      sa[54] = 0.32552984f;
      sa[55] = 0.08255823f;
      sa[56] = -0.21516597f;
      sa[57] = -0.53708994f;
      sa[58] = 0.6123715f;
      sa[59] = -0.27734742f;
      sa[60] = -0.44581077f;
      sa[61] = 0.22526191f;
      sa[62] = 0.14636292f;
      sa[63] = -0.08128333f;
      sa[64] = 0.05071651f;
      sa[65] = 0.11811092f;
      sa[66] = 0.06609037f;
      sa[67] = 0.2328274f;
      sa[68] = 0.088418365f;
      sa[69] = 0.42613938f;
      sa[70] = -0.42483574f;
      sa[71] = 0.8963466f;
      sa[72] = -0.2039633f;
      sa[73] = 0.25619957f;
      sa[74] = -0.31670174f;
      sa[75] = 0.06324335f;
      sa[76] = -0.41350064f;
      sa[77] = -0.168758f;
      sa[78] = -0.09929295f;
      sa[79] = 0.26440063f;
      sa[80] = -0.27603635f;
      sa[81] = -0.1664581f;
      sa[82] = -0.10074148f;
      sa[83] = 0.17885874f;
      sa[84] = -0.08332202f;
      sa[85] = -0.24298427f;
      sa[86] = 0.24134088f;
      sa[87] = 0.3639337f;
      sa[88] = 0.24310766f;
      sa[89] = 0.051159754f;
      sa[90] = -0.17799905f;
      sa[91] = 0.12422087f;
      sa[92] = 0.1308925f;
      sa[93] = -0.33419925f;
      sa[94] = -0.17882596f;
      sa[95] = -0.34408855f;
      sa[96] = 0.07934935f;
      sa[97] = 0.2069065f;
      sa[98] = 0.28536445f;
      sa[99] = -0.198257f;
      sa[100] = -0.14905514f;
      sa[101] = -0.23669562f;
      sa[102] = -0.5652198f;
      sa[103] = 0.060837734f;
      sa[104] = 0.05608159f;
      sa[105] = -0.13095884f;
      sa[106] = -0.32099816f;
      sa[107] = 0.13192925f;
      sa[108] = 0.28565356f;
      sa[109] = -0.28291672f;
      sa[110] = -0.3678178f;
      sa[111] = 0.22146903f;
      sa[112] = 0.20205487f;
      sa[113] = 0.08557763f;
      sa[114] = -0.2740888f;
      sa[115] = 0.043664273f;
      sa[116] = -0.4789835f;
      sa[117] = 0.018089535f;
      sa[118] = -0.4606414f;
      sa[119] = -0.3074723f;
      sa[120] = 0.31211776f;
      sa[121] = 0.43941393f;
      sa[122] = 0.3716054f;
      sa[123] = 0.4773742f;
      sa[124] = 0.39916235f;
      sa[125] = 0.39666593f;
      sa[126] = -0.40112433f;
      sa[127] = -0.114958f;
      sa[128] = 0.17249535f;
      sa[129] = 0.22777943f;
      sa[130] = -0.27831045f;
      sa[131] = 0.48396745f;
      sa[132] = 0.24265577f;
      sa[133] = -0.38033906f;
      sa[134] = 0.0873882f;
      sa[135] = -0.37652674f;
      sa[136] = -0.29434487f;
      sa[137] = -0.05440841f;
      sa[138] = 0.13966711f;
      sa[139] = -0.17523637f;
      sa[140] = -0.28697905f;
      sa[141] = -0.33345985f;
      sa[142] = 0.24613531f;
      sa[143] = -0.4888269f;
      sa[144] = 0.43588915f;
      sa[145] = -0.2494182f;
      sa[146] = -0.3907529f;
      sa[147] = 0.18294095f;
      sa[148] = 0.27013725f;
      sa[149] = 0.30268875f;
      sa[150] = 0.3913904f;
      sa[151] = -0.40133408f;
      sa[152] = 0.036455195f;
      sa[153] = 0.3122028f;
      sa[154] = 0.2447124f;
      sa[155] = -0.18066652f;
      sa[156] = 0.15334006f;
      sa[157] = -0.31322995f;
      sa[158] = 0.1086143f;
      sa[159] = 0.13307245f;
      sa[160] = 0.046599165f;
      sa[161] = 0.29756898f;
      sa[162] = -0.18223058f;
      sa[163] = -0.12497099f;
      sa[164] = 0.3309485f;
      sa[165] = -0.06633481f;
      sa[166] = -0.13228843f;
      sa[167] = -0.2454672f;
      sa[168] = 0.32030755f;
      sa[169] = 0.43173608f;
      sa[170] = -0.052785967f;
      sa[171] = -0.11058596f;
      sa[172] = -0.14269385f;
      sa[173] = -0.26621494f;
      sa[174] = -0.043168824f;
      sa[175] = -0.28139946f;
      sa[176] = -0.10245752f;
      sa[177] = -0.10352599f;
      sa[178] = -0.025908804f;
      sa[179] = 0.25719315f;
      sa[180] = 0.1259592f;
      sa[181] = -0.63606876f;
      sa[182] = -0.18758018f;
      sa[183] = 0.19324392f;
      sa[184] = 0.49548322f;
      sa[185] = 0.354725f;
      sa[186] = -0.27175012f;
      sa[187] = 0.038939137f;
      sa[188] = -0.014446279f;
      sa[189] = 0.33795795f;
      sa[190] = 0.08197354f;
      sa[191] = -0.13017038f;
      sa[192] = -0.32682487f;
      sa[193] = 0.23854561f;
      sa[194] = 0.124232724f;
      sa[195] = 0.42182755f;
      sa[196] = 0.3321705f;
      sa[197] = -0.020437382f;
      sa[198] = 0.21349981f;
      sa[199] = 0.003678333f;
      sa[200] = -0.08853954f;
      sa[201] = -0.308549f;
      sa[202] = 0.2153002f;
      sa[203] = -0.03504697f;
      sa[204] = -0.31690073f;
      sa[205] = 0.5261684f;
      sa[206] = 0.3162201f;
      sa[207] = 0.21119052f;
    }
  }
}
// Neuron weights connecting Tanh and Tanh layer
class h2o_nn_16x16x6_Tanh_12_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[256];
  static {
    h2o_nn_16x16x6_Tanh_12_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.4038534f;
      sa[1] = -0.08582082f;
      sa[2] = -0.3323524f;
      sa[3] = -0.36573467f;
      sa[4] = -0.023822363f;
      sa[5] = -0.13030346f;
      sa[6] = 0.31244513f;
      sa[7] = -0.13194676f;
      sa[8] = -0.456563f;
      sa[9] = 0.2127631f;
      sa[10] = 0.07352013f;
      sa[11] = -0.0073152552f;
      sa[12] = 0.27959156f;
      sa[13] = -0.09365355f;
      sa[14] = -0.40234306f;
      sa[15] = 0.41805792f;
      sa[16] = 0.64812726f;
      sa[17] = -0.14146917f;
      sa[18] = -0.12921515f;
      sa[19] = -0.30194438f;
      sa[20] = 0.443545f;
      sa[21] = 0.13371333f;
      sa[22] = -0.4143666f;
      sa[23] = 0.32288653f;
      sa[24] = -0.3636176f;
      sa[25] = 0.22867648f;
      sa[26] = 0.0695246f;
      sa[27] = 0.31511983f;
      sa[28] = -0.26066917f;
      sa[29] = -0.31270412f;
      sa[30] = 0.09315646f;
      sa[31] = -0.24042459f;
      sa[32] = -0.10406842f;
      sa[33] = -0.4622643f;
      sa[34] = -0.1040755f;
      sa[35] = 0.18791719f;
      sa[36] = 0.010282325f;
      sa[37] = 0.05073654f;
      sa[38] = -0.07561075f;
      sa[39] = 0.42301726f;
      sa[40] = -0.26627374f;
      sa[41] = -0.32778668f;
      sa[42] = -0.096105516f;
      sa[43] = -0.49999702f;
      sa[44] = 0.09735256f;
      sa[45] = -0.5129969f;
      sa[46] = 0.3641987f;
      sa[47] = 0.26690125f;
      sa[48] = -0.2809818f;
      sa[49] = -0.30653298f;
      sa[50] = 0.16647284f;
      sa[51] = -0.17569648f;
      sa[52] = 0.24408244f;
      sa[53] = 0.23452276f;
      sa[54] = -0.106911145f;
      sa[55] = 0.49838623f;
      sa[56] = -0.33107632f;
      sa[57] = -0.32392234f;
      sa[58] = 0.10607995f;
      sa[59] = -0.12010592f;
      sa[60] = 0.38358837f;
      sa[61] = -0.18224435f;
      sa[62] = -0.045568332f;
      sa[63] = 0.006042494f;
      sa[64] = 0.3566644f;
      sa[65] = -0.1194038f;
      sa[66] = 0.12117451f;
      sa[67] = 0.3986214f;
      sa[68] = 0.31495765f;
      sa[69] = -0.008379837f;
      sa[70] = -0.46071008f;
      sa[71] = -0.11505459f;
      sa[72] = -0.34009236f;
      sa[73] = 0.37441075f;
      sa[74] = 0.22184706f;
      sa[75] = 0.005457864f;
      sa[76] = 0.11699347f;
      sa[77] = 0.12024897f;
      sa[78] = -0.4395543f;
      sa[79] = 0.12681372f;
      sa[80] = -0.2143772f;
      sa[81] = 0.008792481f;
      sa[82] = 0.36501205f;
      sa[83] = -0.11122694f;
      sa[84] = -0.59129304f;
      sa[85] = -0.64215416f;
      sa[86] = 0.15400529f;
      sa[87] = 0.3871204f;
      sa[88] = -0.44043455f;
      sa[89] = -0.24873117f;
      sa[90] = -0.20014554f;
      sa[91] = 0.39475164f;
      sa[92] = -0.023053285f;
      sa[93] = 0.13172261f;
      sa[94] = -0.28214818f;
      sa[95] = 0.008734796f;
      sa[96] = -0.13406637f;
      sa[97] = -5.1263877E-4f;
      sa[98] = -0.16969311f;
      sa[99] = -0.117622875f;
      sa[100] = -0.2742881f;
      sa[101] = -0.061440986f;
      sa[102] = 0.2434329f;
      sa[103] = 0.17709157f;
      sa[104] = -0.17549306f;
      sa[105] = -0.12412743f;
      sa[106] = 0.54078805f;
      sa[107] = 0.19855666f;
      sa[108] = 0.3254924f;
      sa[109] = -0.08743787f;
      sa[110] = 0.2119306f;
      sa[111] = -0.31474072f;
      sa[112] = -0.3748796f;
      sa[113] = 0.0113085685f;
      sa[114] = 0.3364376f;
      sa[115] = -0.07807128f;
      sa[116] = 0.30950713f;
      sa[117] = -0.14327902f;
      sa[118] = -0.27180383f;
      sa[119] = -0.089504376f;
      sa[120] = -0.19534338f;
      sa[121] = 0.20618473f;
      sa[122] = -0.046289887f;
      sa[123] = 0.09652462f;
      sa[124] = -0.250483f;
      sa[125] = 0.24638143f;
      sa[126] = -0.15701278f;
      sa[127] = 0.019762764f;
      sa[128] = -0.25846615f;
      sa[129] = 0.20131412f;
      sa[130] = 0.13001592f;
      sa[131] = 0.2638417f;
      sa[132] = 0.12993838f;
      sa[133] = 0.1397918f;
      sa[134] = 0.20777558f;
      sa[135] = -0.2048827f;
      sa[136] = 0.2458875f;
      sa[137] = -0.24194269f;
      sa[138] = -0.09016887f;
      sa[139] = 0.37628952f;
      sa[140] = -0.27232373f;
      sa[141] = 0.08425551f;
      sa[142] = 0.17705372f;
      sa[143] = -0.37663278f;
      sa[144] = 0.2461392f;
      sa[145] = -0.23381147f;
      sa[146] = 0.2640302f;
      sa[147] = 0.00567949f;
      sa[148] = -0.3169147f;
      sa[149] = 0.11019575f;
      sa[150] = -0.23494816f;
      sa[151] = -0.4886325f;
      sa[152] = 0.53477806f;
      sa[153] = -0.19847852f;
      sa[154] = 0.4023184f;
      sa[155] = -0.37988886f;
      sa[156] = -0.38531876f;
      sa[157] = -0.23699865f;
      sa[158] = 0.04472029f;
      sa[159] = -0.03591638f;
      sa[160] = 0.4664355f;
      sa[161] = -0.36357176f;
      sa[162] = 0.5772134f;
      sa[163] = -0.25915217f;
      sa[164] = 0.17122933f;
      sa[165] = -0.25694248f;
      sa[166] = 0.34630308f;
      sa[167] = -0.20450693f;
      sa[168] = 0.3153839f;
      sa[169] = -0.5667307f;
      sa[170] = 0.3689052f;
      sa[171] = 0.18737365f;
      sa[172] = 0.36903724f;
      sa[173] = 0.46376464f;
      sa[174] = 0.08764216f;
      sa[175] = -0.1628757f;
      sa[176] = -0.37030363f;
      sa[177] = 0.04604952f;
      sa[178] = -0.15258394f;
      sa[179] = 0.082251586f;
      sa[180] = -0.5667901f;
      sa[181] = -0.3918061f;
      sa[182] = 0.21783057f;
      sa[183] = -0.37857896f;
      sa[184] = 0.34483793f;
      sa[185] = 0.24084349f;
      sa[186] = -0.23626007f;
      sa[187] = -0.20488599f;
      sa[188] = -0.1520623f;
      sa[189] = -0.03980834f;
      sa[190] = -0.3859578f;
      sa[191] = -0.1899514f;
      sa[192] = -0.29522043f;
      sa[193] = 0.18968523f;
      sa[194] = -0.1709836f;
      sa[195] = 0.012268507f;
      sa[196] = 0.18497814f;
      sa[197] = 0.0099845035f;
      sa[198] = 0.11777969f;
      sa[199] = 0.28942192f;
      sa[200] = 0.13361427f;
      sa[201] = 0.27383995f;
      sa[202] = -0.03831038f;
      sa[203] = -0.23860656f;
      sa[204] = -0.034196243f;
      sa[205] = 0.50081795f;
      sa[206] = -0.23047842f;
      sa[207] = 0.12776098f;
      sa[208] = -0.25403488f;
      sa[209] = 0.12781614f;
      sa[210] = -0.067760356f;
      sa[211] = -0.25016358f;
      sa[212] = -0.48170424f;
      sa[213] = -0.20232217f;
      sa[214] = -0.2628274f;
      sa[215] = -0.011214524f;
      sa[216] = -0.025974665f;
      sa[217] = 0.23162635f;
      sa[218] = 0.088077895f;
      sa[219] = 0.06806108f;
      sa[220] = 0.26921386f;
      sa[221] = -0.042566694f;
      sa[222] = -0.24039227f;
      sa[223] = -0.24115375f;
      sa[224] = 0.25318396f;
      sa[225] = 0.037638888f;
      sa[226] = -0.028071823f;
      sa[227] = 0.10923417f;
      sa[228] = -0.22731856f;
      sa[229] = -0.38197702f;
      sa[230] = -0.1972243f;
      sa[231] = -0.17294039f;
      sa[232] = 0.36043096f;
      sa[233] = -0.19119759f;
      sa[234] = -0.069941215f;
      sa[235] = 0.046430856f;
      sa[236] = -0.19533291f;
      sa[237] = 0.41565928f;
      sa[238] = 0.19317602f;
      sa[239] = 0.06358223f;
      sa[240] = -0.03333007f;
      sa[241] = -0.35850424f;
      sa[242] = -0.4374692f;
      sa[243] = 0.37947667f;
      sa[244] = 0.21866421f;
      sa[245] = 0.46902683f;
      sa[246] = 0.10977257f;
      sa[247] = -0.44934753f;
      sa[248] = -0.01214847f;
      sa[249] = 0.06040264f;
      sa[250] = 0.20720266f;
      sa[251] = 0.44867438f;
      sa[252] = 0.16534232f;
      sa[253] = -0.3260048f;
      sa[254] = 0.3323215f;
      sa[255] = 0.26360124f;
    }
  }
}
// Neuron weights connecting Tanh and Softmax layer
class h2o_nn_16x16x6_Tanh_12_Weight_3 implements java.io.Serializable {
  public static final float[] VALUES = new float[80];
  static {
    h2o_nn_16x16x6_Tanh_12_Weight_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_Weight_3_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = -0.041512556f;
      sa[1] = -1.6027428f;
      sa[2] = -1.5977345f;
      sa[3] = 1.1846054f;
      sa[4] = 0.23645534f;
      sa[5] = 0.07071973f;
      sa[6] = -1.6075014f;
      sa[7] = 1.5624677f;
      sa[8] = 2.069611f;
      sa[9] = 0.15716791f;
      sa[10] = 1.8818586f;
      sa[11] = 1.4864514f;
      sa[12] = 1.8639112f;
      sa[13] = -0.6560885f;
      sa[14] = -1.7577022f;
      sa[15] = -0.80855286f;
      sa[16] = 0.7162861f;
      sa[17] = -0.8261685f;
      sa[18] = -1.5330353f;
      sa[19] = -1.1974366f;
      sa[20] = -1.8871098f;
      sa[21] = 0.33848214f;
      sa[22] = -1.9740869f;
      sa[23] = -0.6198624f;
      sa[24] = -0.5052044f;
      sa[25] = 1.8843542f;
      sa[26] = -0.30493224f;
      sa[27] = 1.2475564f;
      sa[28] = 1.1885792f;
      sa[29] = -0.7722101f;
      sa[30] = 0.21730286f;
      sa[31] = 0.19856603f;
      sa[32] = 0.59457475f;
      sa[33] = 0.5504036f;
      sa[34] = -0.6726119f;
      sa[35] = 1.1074659f;
      sa[36] = 0.52719873f;
      sa[37] = -1.5145363f;
      sa[38] = 0.48765337f;
      sa[39] = -0.22808205f;
      sa[40] = 0.17539679f;
      sa[41] = -0.6523266f;
      sa[42] = 0.41550747f;
      sa[43] = -0.16510718f;
      sa[44] = 1.0213648f;
      sa[45] = -1.9838063f;
      sa[46] = -2.1823196f;
      sa[47] = 0.03170887f;
      sa[48] = 1.8258555f;
      sa[49] = -0.19970651f;
      sa[50] = -0.37955394f;
      sa[51] = 1.9476123f;
      sa[52] = -1.655051f;
      sa[53] = 1.0824512f;
      sa[54] = 1.9776157f;
      sa[55] = 1.0220494f;
      sa[56] = -0.70332295f;
      sa[57] = -0.06373132f;
      sa[58] = 1.134261f;
      sa[59] = 0.7293925f;
      sa[60] = -1.3587316f;
      sa[61] = 1.0217011f;
      sa[62] = -0.48646954f;
      sa[63] = -0.9460592f;
      sa[64] = 1.0786875f;
      sa[65] = -1.0583872f;
      sa[66] = -1.7935026f;
      sa[67] = -0.15859556f;
      sa[68] = 0.9053671f;
      sa[69] = -1.3680449f;
      sa[70] = -1.7569939f;
      sa[71] = 0.1167402f;
      sa[72] = -0.93540883f;
      sa[73] = -1.3241343f;
      sa[74] = -1.3720139f;
      sa[75] = 1.3562189f;
      sa[76] = 1.863123f;
      sa[77] = -0.6126967f;
      sa[78] = -1.5185059f;
      sa[79] = -0.24341995f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_16x16x6_Tanh_12 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_16x16x6_Tanh_12_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_16x16x6_Tanh_12_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_12_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    h2o_nn_16x16x6_Tanh_12_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_12_ColInfo_13_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "4";
      sa[3] = "5";
      sa[4] = "6";
    }
  }
}


