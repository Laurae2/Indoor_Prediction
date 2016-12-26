/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:42:55.991+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_16x16x6_Tanh_09

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_16x16x6_Tanh_09 > h2o_nn_16x16x6_Tanh_09.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_16x16x6_Tanh_09.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_16x16x6_Tanh_09", algorithm="deeplearning")
public class h2o_nn_16x16x6_Tanh_09 extends GenModel {
  public hex.ModelCategory getModelCategory() { return hex.ModelCategory.Multinomial; }
  public boolean isSupervised() { return true; }
  public int nfeatures() { return 13; }
  public int nclasses() { return 6; }
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
  public static final int[] NEURONS = {13,16,16,6};
    // Thread-local storage for neuron activation values.
    final double[][] ACTIVATION = new double[][] {
      /* Input */ h2o_nn_16x16x6_Tanh_09_Activation_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_09_Activation_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_09_Activation_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_09_Activation_3.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_16x16x6_Tanh_09_Bias_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_09_Bias_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_09_Bias_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_09_Bias_3.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_16x16x6_Tanh_09_Weight_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_09_Weight_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_09_Weight_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_09_Weight_3.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_16x16x6_Tanh_09.VALUES;
  // Number of output classes included in training data response column.
  public static final int NCLASSES = 6;

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
    /* Label */ h2o_nn_16x16x6_Tanh_09_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.25961538461538464,0.11538461538461539,0.11538461538461539,0.125,0.25961538461538464,0.125};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_16x16x6_Tanh_09() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(4320118218852882336L); }

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
class h2o_nn_16x16x6_Tanh_09_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_16x16x6_Tanh_09_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_Activation_0_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_09_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_09_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_Activation_1_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_09_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_09_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_Activation_2_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_09_Activation_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_16x16x6_Tanh_09_Activation_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_Activation_3_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0;
      sa[1] = 0.0;
      sa[2] = 0.0;
      sa[3] = 0.0;
      sa[4] = 0.0;
      sa[5] = 0.0;
    }
  }
}
// Neuron bias values for Input layer
class h2o_nn_16x16x6_Tanh_09_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Tanh layer
class h2o_nn_16x16x6_Tanh_09_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_09_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = -0.025368815640762588;
      sa[1] = 0.02595514143332845;
      sa[2] = 0.007192265901037322;
      sa[3] = 0.0567053356952493;
      sa[4] = 0.06993196456815094;
      sa[5] = 0.05626896917700131;
      sa[6] = 0.016720797641107964;
      sa[7] = -0.14521993508281736;
      sa[8] = 0.05506894575854124;
      sa[9] = 0.018657173212822217;
      sa[10] = -0.017947630497465027;
      sa[11] = -0.0227321984782529;
      sa[12] = -0.025336626762190624;
      sa[13] = -0.0594189942576308;
      sa[14] = -0.0949344117281814;
      sa[15] = -0.04602993682881571;
    }
  }
}
// Neuron bias values for Tanh layer
class h2o_nn_16x16x6_Tanh_09_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_09_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.042477091083466686;
      sa[1] = -0.018231190004472698;
      sa[2] = -0.130594752328006;
      sa[3] = 0.08185270515493284;
      sa[4] = -0.08740839310050925;
      sa[5] = 0.02221804730846594;
      sa[6] = -0.03863667070560268;
      sa[7] = 0.14277578075717856;
      sa[8] = 0.011665921091845449;
      sa[9] = 0.04013164208420234;
      sa[10] = 0.013747858616017856;
      sa[11] = 0.23533194974802443;
      sa[12] = -0.025775377767527124;
      sa[13] = -0.08443699302806372;
      sa[14] = -0.060676109159545355;
      sa[15] = -0.10093978201952201;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_16x16x6_Tanh_09_Bias_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_16x16x6_Tanh_09_Bias_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_Bias_3_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.04622237446088061;
      sa[1] = -0.044760051560234125;
      sa[2] = -0.11965196726143466;
      sa[3] = 0.013279725242772855;
      sa[4] = 0.029724207292446868;
      sa[5] = -0.17904104295576473;
    }
  }
}
class h2o_nn_16x16x6_Tanh_09_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Tanh layer
class h2o_nn_16x16x6_Tanh_09_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[208];
  static {
    h2o_nn_16x16x6_Tanh_09_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.19752233f;
      sa[1] = 0.44468224f;
      sa[2] = 0.40570983f;
      sa[3] = 0.23525201f;
      sa[4] = -0.11616188f;
      sa[5] = -0.19906859f;
      sa[6] = -0.25798652f;
      sa[7] = 0.44000778f;
      sa[8] = 0.37695214f;
      sa[9] = -0.22518784f;
      sa[10] = -0.3129697f;
      sa[11] = -0.29606092f;
      sa[12] = 0.15861754f;
      sa[13] = -0.33254302f;
      sa[14] = -0.41531134f;
      sa[15] = -0.02199423f;
      sa[16] = 0.10900001f;
      sa[17] = 0.269899f;
      sa[18] = -0.25887704f;
      sa[19] = 0.24767715f;
      sa[20] = 0.35205707f;
      sa[21] = -0.46803355f;
      sa[22] = 0.45894834f;
      sa[23] = -0.1387897f;
      sa[24] = 0.3469933f;
      sa[25] = 0.3239552f;
      sa[26] = -0.49844426f;
      sa[27] = 0.48290172f;
      sa[28] = 0.2803047f;
      sa[29] = -0.38965058f;
      sa[30] = -0.4065549f;
      sa[31] = 0.1749935f;
      sa[32] = 0.3071109f;
      sa[33] = -0.46017855f;
      sa[34] = 0.113425136f;
      sa[35] = -0.1700158f;
      sa[36] = 0.0802861f;
      sa[37] = 0.48302934f;
      sa[38] = -0.26279905f;
      sa[39] = 0.23462623f;
      sa[40] = -0.009460695f;
      sa[41] = -0.14361447f;
      sa[42] = -0.4738163f;
      sa[43] = -0.047212537f;
      sa[44] = 0.44486323f;
      sa[45] = -0.50787765f;
      sa[46] = 0.07560709f;
      sa[47] = 0.042656347f;
      sa[48] = 0.3585641f;
      sa[49] = 0.41654015f;
      sa[50] = 0.40653312f;
      sa[51] = -0.44387594f;
      sa[52] = -0.18617792f;
      sa[53] = 0.3680414f;
      sa[54] = 0.2610384f;
      sa[55] = -0.17559738f;
      sa[56] = -0.42771372f;
      sa[57] = -0.4084794f;
      sa[58] = 0.2980666f;
      sa[59] = -0.2903091f;
      sa[60] = -0.43790555f;
      sa[61] = 0.27079725f;
      sa[62] = 0.11567228f;
      sa[63] = 0.09704269f;
      sa[64] = 0.085384205f;
      sa[65] = 0.27687174f;
      sa[66] = 0.3040002f;
      sa[67] = 0.39530197f;
      sa[68] = -0.036318436f;
      sa[69] = 0.26291916f;
      sa[70] = -0.2936512f;
      sa[71] = 0.47819358f;
      sa[72] = -0.17208667f;
      sa[73] = 0.438013f;
      sa[74] = -0.3942971f;
      sa[75] = -0.25455132f;
      sa[76] = -0.23473485f;
      sa[77] = -0.28909114f;
      sa[78] = -0.16398f;
      sa[79] = 0.33202794f;
      sa[80] = -0.31806976f;
      sa[81] = -0.26301903f;
      sa[82] = -0.19992091f;
      sa[83] = 0.2329535f;
      sa[84] = -0.11856099f;
      sa[85] = -0.29346314f;
      sa[86] = 0.21871305f;
      sa[87] = 0.34090596f;
      sa[88] = 0.1717261f;
      sa[89] = 0.16249445f;
      sa[90] = -0.24444285f;
      sa[91] = 0.0033352643f;
      sa[92] = 0.3973155f;
      sa[93] = -0.30672172f;
      sa[94] = -0.33581752f;
      sa[95] = -0.54687476f;
      sa[96] = 0.1766237f;
      sa[97] = -0.06578276f;
      sa[98] = 0.24980485f;
      sa[99] = -0.14001106f;
      sa[100] = -0.13476774f;
      sa[101] = -0.4096512f;
      sa[102] = -0.469686f;
      sa[103] = 0.03719317f;
      sa[104] = 0.1260613f;
      sa[105] = -0.21342133f;
      sa[106] = -0.29326856f;
      sa[107] = 0.15661648f;
      sa[108] = 0.431201f;
      sa[109] = -0.23880379f;
      sa[110] = -0.25964367f;
      sa[111] = 0.21047576f;
      sa[112] = 0.19379444f;
      sa[113] = 0.108631514f;
      sa[114] = -0.3013558f;
      sa[115] = 0.09280257f;
      sa[116] = -0.44036743f;
      sa[117] = -0.017295377f;
      sa[118] = -0.53163296f;
      sa[119] = -0.2751375f;
      sa[120] = 0.2618193f;
      sa[121] = 0.33719206f;
      sa[122] = 0.5200454f;
      sa[123] = 0.32354906f;
      sa[124] = 0.39054438f;
      sa[125] = 0.3846252f;
      sa[126] = -0.31711802f;
      sa[127] = -0.14603932f;
      sa[128] = 0.06576179f;
      sa[129] = 0.32880962f;
      sa[130] = -0.3092302f;
      sa[131] = 0.6796274f;
      sa[132] = 0.23052427f;
      sa[133] = -0.49080074f;
      sa[134] = -0.073542506f;
      sa[135] = -0.3677317f;
      sa[136] = -0.4802919f;
      sa[137] = -0.049790896f;
      sa[138] = 0.18382165f;
      sa[139] = -0.1927451f;
      sa[140] = -0.26606038f;
      sa[141] = -0.12580441f;
      sa[142] = 0.16830336f;
      sa[143] = -0.433363f;
      sa[144] = 0.42641646f;
      sa[145] = -0.24281164f;
      sa[146] = -0.30887616f;
      sa[147] = 0.23865893f;
      sa[148] = 0.1311055f;
      sa[149] = 0.41940346f;
      sa[150] = 0.43161374f;
      sa[151] = -0.37639463f;
      sa[152] = -0.003493974f;
      sa[153] = 0.47290266f;
      sa[154] = 0.34112132f;
      sa[155] = -0.32463667f;
      sa[156] = 0.085544094f;
      sa[157] = -0.26138487f;
      sa[158] = 0.07330404f;
      sa[159] = 0.08218469f;
      sa[160] = -0.102314346f;
      sa[161] = 0.26881516f;
      sa[162] = -0.23686978f;
      sa[163] = -0.15293695f;
      sa[164] = 0.29314715f;
      sa[165] = -0.057475302f;
      sa[166] = -0.04422805f;
      sa[167] = -0.15111318f;
      sa[168] = 0.27114722f;
      sa[169] = 0.32157025f;
      sa[170] = -0.25496918f;
      sa[171] = -0.38379765f;
      sa[172] = -0.1495426f;
      sa[173] = -0.3094949f;
      sa[174] = -0.025256013f;
      sa[175] = -0.25049406f;
      sa[176] = -0.124341786f;
      sa[177] = -0.26257694f;
      sa[178] = 0.038600106f;
      sa[179] = 0.49918574f;
      sa[180] = 0.12533644f;
      sa[181] = -0.42553023f;
      sa[182] = -0.1581083f;
      sa[183] = 0.38368014f;
      sa[184] = 0.58979106f;
      sa[185] = 0.24755159f;
      sa[186] = -0.35056525f;
      sa[187] = 0.12522782f;
      sa[188] = -0.15456556f;
      sa[189] = 0.36821052f;
      sa[190] = 0.19020219f;
      sa[191] = -0.15435593f;
      sa[192] = -0.42806217f;
      sa[193] = 0.43194622f;
      sa[194] = 0.043142974f;
      sa[195] = 0.25554812f;
      sa[196] = 0.35095528f;
      sa[197] = -0.11237149f;
      sa[198] = 0.061156366f;
      sa[199] = -0.32923013f;
      sa[200] = 0.011107246f;
      sa[201] = -0.529047f;
      sa[202] = 0.21896498f;
      sa[203] = -0.050243996f;
      sa[204] = -0.23305917f;
      sa[205] = 0.49650806f;
      sa[206] = 0.36094883f;
      sa[207] = 0.28292775f;
    }
  }
}
// Neuron weights connecting Tanh and Tanh layer
class h2o_nn_16x16x6_Tanh_09_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[256];
  static {
    h2o_nn_16x16x6_Tanh_09_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.5984081f;
      sa[1] = -0.032236952f;
      sa[2] = -0.39310914f;
      sa[3] = -0.5148738f;
      sa[4] = 0.07484311f;
      sa[5] = 0.12089072f;
      sa[6] = 0.07393413f;
      sa[7] = -0.08264514f;
      sa[8] = -0.5114065f;
      sa[9] = 0.41438407f;
      sa[10] = 0.014410449f;
      sa[11] = -0.11730148f;
      sa[12] = 0.26301318f;
      sa[13] = -0.16217111f;
      sa[14] = -0.4804249f;
      sa[15] = 0.25560847f;
      sa[16] = 0.7081068f;
      sa[17] = -0.2154474f;
      sa[18] = 0.046553962f;
      sa[19] = -0.24766444f;
      sa[20] = 0.33950794f;
      sa[21] = 0.14286315f;
      sa[22] = -0.34822035f;
      sa[23] = 0.30577388f;
      sa[24] = -0.296762f;
      sa[25] = -1.17805E-5f;
      sa[26] = 0.095940635f;
      sa[27] = 0.32059786f;
      sa[28] = -0.43853614f;
      sa[29] = -0.13385442f;
      sa[30] = 0.28053448f;
      sa[31] = -0.12762994f;
      sa[32] = -0.19085471f;
      sa[33] = -0.28234065f;
      sa[34] = -0.077278286f;
      sa[35] = 0.12250972f;
      sa[36] = 0.012706822f;
      sa[37] = 0.21461685f;
      sa[38] = -0.22380035f;
      sa[39] = 0.08860823f;
      sa[40] = -0.14075075f;
      sa[41] = -0.37375352f;
      sa[42] = -0.32865104f;
      sa[43] = -0.37365553f;
      sa[44] = -0.07096048f;
      sa[45] = -0.394983f;
      sa[46] = 0.17076983f;
      sa[47] = 0.35856843f;
      sa[48] = -0.17224105f;
      sa[49] = -0.35915548f;
      sa[50] = 0.15974939f;
      sa[51] = -0.22807764f;
      sa[52] = 0.31029835f;
      sa[53] = 0.3332004f;
      sa[54] = -0.10581196f;
      sa[55] = 0.46065688f;
      sa[56] = -0.26105905f;
      sa[57] = -0.39720073f;
      sa[58] = 0.10529066f;
      sa[59] = -0.08914587f;
      sa[60] = 0.2901036f;
      sa[61] = -0.15928012f;
      sa[62] = -0.028738968f;
      sa[63] = 0.13113345f;
      sa[64] = 0.11417481f;
      sa[65] = -0.0439229f;
      sa[66] = 0.1946497f;
      sa[67] = 0.5273983f;
      sa[68] = 0.22787827f;
      sa[69] = -0.49787322f;
      sa[70] = -0.2384798f;
      sa[71] = -0.17937738f;
      sa[72] = -0.37312415f;
      sa[73] = 0.09277012f;
      sa[74] = 0.32202956f;
      sa[75] = 0.08062412f;
      sa[76] = 0.17909315f;
      sa[77] = 0.23747408f;
      sa[78] = -0.54214877f;
      sa[79] = 0.41144434f;
      sa[80] = -0.2560629f;
      sa[81] = 0.19839485f;
      sa[82] = 0.08680896f;
      sa[83] = -0.34659654f;
      sa[84] = -0.38454616f;
      sa[85] = -0.07569737f;
      sa[86] = -0.1226234f;
      sa[87] = 0.16502196f;
      sa[88] = -0.37464043f;
      sa[89] = -0.003714191f;
      sa[90] = -0.51111984f;
      sa[91] = 0.40007946f;
      sa[92] = -0.09689109f;
      sa[93] = 0.023516428f;
      sa[94] = -0.6069077f;
      sa[95] = -0.16525722f;
      sa[96] = 0.011403023f;
      sa[97] = 0.03029235f;
      sa[98] = -0.06624452f;
      sa[99] = -0.28221375f;
      sa[100] = -0.22944334f;
      sa[101] = 0.112436466f;
      sa[102] = 0.15974654f;
      sa[103] = 0.048270527f;
      sa[104] = -0.020062948f;
      sa[105] = -0.21347244f;
      sa[106] = 0.48602495f;
      sa[107] = 0.2457132f;
      sa[108] = 0.14308912f;
      sa[109] = -0.03314846f;
      sa[110] = 0.20913668f;
      sa[111] = -0.21804357f;
      sa[112] = -0.43415052f;
      sa[113] = 0.17796461f;
      sa[114] = -0.05431823f;
      sa[115] = -0.19256237f;
      sa[116] = 0.41231152f;
      sa[117] = 0.14809312f;
      sa[118] = -0.44332004f;
      sa[119] = -0.2783999f;
      sa[120] = -0.0877617f;
      sa[121] = 0.32303548f;
      sa[122] = -0.21715982f;
      sa[123] = 0.11822493f;
      sa[124] = -0.30155703f;
      sa[125] = 0.25963598f;
      sa[126] = -0.7044289f;
      sa[127] = -0.056313246f;
      sa[128] = -0.36980748f;
      sa[129] = 0.17603216f;
      sa[130] = 0.15177804f;
      sa[131] = 0.33456808f;
      sa[132] = 0.12042701f;
      sa[133] = 0.13754624f;
      sa[134] = 0.31655824f;
      sa[135] = -0.26348692f;
      sa[136] = 0.25403056f;
      sa[137] = -0.33001947f;
      sa[138] = -0.06967243f;
      sa[139] = 0.50745386f;
      sa[140] = -0.27039555f;
      sa[141] = 0.083294526f;
      sa[142] = 0.16751172f;
      sa[143] = -0.22913712f;
      sa[144] = 0.21559027f;
      sa[145] = -0.26001114f;
      sa[146] = 0.20931384f;
      sa[147] = 0.053398333f;
      sa[148] = -0.32217413f;
      sa[149] = 0.390074f;
      sa[150] = -0.19137214f;
      sa[151] = -0.47707355f;
      sa[152] = 0.52712816f;
      sa[153] = -0.04463929f;
      sa[154] = 0.28061345f;
      sa[155] = -0.3119613f;
      sa[156] = -0.3882981f;
      sa[157] = -0.27278978f;
      sa[158] = 0.30960947f;
      sa[159] = -0.23152024f;
      sa[160] = 0.23341006f;
      sa[161] = -0.25565833f;
      sa[162] = 0.50775176f;
      sa[163] = -0.20236908f;
      sa[164] = 0.2634865f;
      sa[165] = 0.012457902f;
      sa[166] = 0.42830423f;
      sa[167] = -0.4317175f;
      sa[168] = 0.38136148f;
      sa[169] = -0.50015813f;
      sa[170] = 0.16003522f;
      sa[171] = 0.34721324f;
      sa[172] = 0.29885605f;
      sa[173] = 0.4975819f;
      sa[174] = -0.04941018f;
      sa[175] = -0.07728271f;
      sa[176] = -0.32244477f;
      sa[177] = 0.007850665f;
      sa[178] = -0.31700438f;
      sa[179] = -0.024862517f;
      sa[180] = -0.42754313f;
      sa[181] = -0.39506093f;
      sa[182] = 0.1450781f;
      sa[183] = -0.14829837f;
      sa[184] = 0.30522707f;
      sa[185] = 0.4659761f;
      sa[186] = -0.19872344f;
      sa[187] = -0.33007333f;
      sa[188] = 0.01747896f;
      sa[189] = -0.229282f;
      sa[190] = -0.47325104f;
      sa[191] = -0.40817815f;
      sa[192] = -0.31786975f;
      sa[193] = 0.106768414f;
      sa[194] = -0.18514611f;
      sa[195] = -0.039260402f;
      sa[196] = 0.08500841f;
      sa[197] = -0.41137403f;
      sa[198] = 0.28130868f;
      sa[199] = 0.39735705f;
      sa[200] = -0.02554811f;
      sa[201] = 0.15774435f;
      sa[202] = 0.14078015f;
      sa[203] = -0.29335493f;
      sa[204] = 0.124051966f;
      sa[205] = 0.4988293f;
      sa[206] = -0.091581285f;
      sa[207] = 0.19537097f;
      sa[208] = -0.21369214f;
      sa[209] = 0.35476536f;
      sa[210] = -0.34089884f;
      sa[211] = -0.48890448f;
      sa[212] = -0.2626641f;
      sa[213] = 0.19447859f;
      sa[214] = -0.45199752f;
      sa[215] = -0.27592212f;
      sa[216] = 0.094094f;
      sa[217] = 0.4543575f;
      sa[218] = -0.10381842f;
      sa[219] = 0.04798336f;
      sa[220] = 0.16207252f;
      sa[221] = -0.09201969f;
      sa[222] = -0.47961542f;
      sa[223] = -0.41420645f;
      sa[224] = 0.24815685f;
      sa[225] = 0.20677331f;
      sa[226] = -0.2555261f;
      sa[227] = -0.063345306f;
      sa[228] = -0.11961935f;
      sa[229] = -0.10832914f;
      sa[230] = -0.42313328f;
      sa[231] = -0.37857342f;
      sa[232] = 0.40850183f;
      sa[233] = 0.03158805f;
      sa[234] = -0.2555552f;
      sa[235] = 0.0051069157f;
      sa[236] = -0.31222254f;
      sa[237] = 0.33906242f;
      sa[238] = -0.037631165f;
      sa[239] = -0.124630935f;
      sa[240] = -0.20028646f;
      sa[241] = -0.31850147f;
      sa[242] = -0.25735062f;
      sa[243] = 0.47782537f;
      sa[244] = 0.131012f;
      sa[245] = 0.38393143f;
      sa[246] = 0.21203518f;
      sa[247] = -0.6253003f;
      sa[248] = 0.023284102f;
      sa[249] = -8.7570096E-4f;
      sa[250] = 0.17969231f;
      sa[251] = 0.538025f;
      sa[252] = 0.15795575f;
      sa[253] = -0.27154404f;
      sa[254] = 0.42370963f;
      sa[255] = 0.27229246f;
    }
  }
}
// Neuron weights connecting Tanh and Softmax layer
class h2o_nn_16x16x6_Tanh_09_Weight_3 implements java.io.Serializable {
  public static final float[] VALUES = new float[96];
  static {
    h2o_nn_16x16x6_Tanh_09_Weight_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_Weight_3_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.013690966f;
      sa[1] = -1.5471245f;
      sa[2] = -1.5415864f;
      sa[3] = 1.1966331f;
      sa[4] = 0.36198708f;
      sa[5] = -0.086422205f;
      sa[6] = -1.627068f;
      sa[7] = 1.529259f;
      sa[8] = 2.0755665f;
      sa[9] = 0.12322414f;
      sa[10] = 1.7916524f;
      sa[11] = 1.5303619f;
      sa[12] = 1.821446f;
      sa[13] = -0.6565065f;
      sa[14] = -1.8395752f;
      sa[15] = -0.65734506f;
      sa[16] = 0.56814873f;
      sa[17] = -0.81988955f;
      sa[18] = -1.436542f;
      sa[19] = -0.95814735f;
      sa[20] = -1.8835837f;
      sa[21] = 0.24307403f;
      sa[22] = -1.8593355f;
      sa[23] = -0.73354167f;
      sa[24] = -0.35025027f;
      sa[25] = 1.8520852f;
      sa[26] = -0.28818643f;
      sa[27] = 1.2050169f;
      sa[28] = 1.0248082f;
      sa[29] = -0.90676606f;
      sa[30] = 0.17202364f;
      sa[31] = 0.28253025f;
      sa[32] = 0.33011365f;
      sa[33] = 0.31800994f;
      sa[34] = -0.609174f;
      sa[35] = 1.0092065f;
      sa[36] = 0.53799593f;
      sa[37] = -1.4055783f;
      sa[38] = 0.6214988f;
      sa[39] = -0.27373168f;
      sa[40] = 0.3096524f;
      sa[41] = -0.6280451f;
      sa[42] = 0.710633f;
      sa[43] = -0.43301126f;
      sa[44] = 0.89477694f;
      sa[45] = -2.1252298f;
      sa[46] = -2.0143933f;
      sa[47] = 0.12756647f;
      sa[48] = 1.8798901f;
      sa[49] = 0.09914201f;
      sa[50] = -0.5164653f;
      sa[51] = 1.9218086f;
      sa[52] = -1.7951773f;
      sa[53] = 0.89868563f;
      sa[54] = 1.6912742f;
      sa[55] = 1.214321f;
      sa[56] = -0.7040523f;
      sa[57] = 0.0291385f;
      sa[58] = 0.896977f;
      sa[59] = 0.8925679f;
      sa[60] = -1.3495798f;
      sa[61] = 1.1171439f;
      sa[62] = -0.42702878f;
      sa[63] = -0.9345148f;
      sa[64] = 1.399261f;
      sa[65] = -0.9588339f;
      sa[66] = -1.6271753f;
      sa[67] = -0.020634715f;
      sa[68] = 1.0340803f;
      sa[69] = -1.3285184f;
      sa[70] = -1.5660323f;
      sa[71] = -0.16519353f;
      sa[72] = -1.1097573f;
      sa[73] = -1.3142164f;
      sa[74] = -1.1658897f;
      sa[75] = 1.2129079f;
      sa[76] = 1.9338677f;
      sa[77] = -0.62927145f;
      sa[78] = -1.5212811f;
      sa[79] = -0.45703402f;
      sa[80] = 0.7684779f;
      sa[81] = -1.6232392f;
      sa[82] = -0.3205569f;
      sa[83] = -1.538835f;
      sa[84] = 1.5307144f;
      sa[85] = 1.2258539f;
      sa[86] = -1.9782243f;
      sa[87] = 1.389273f;
      sa[88] = -1.4200851f;
      sa[89] = -0.97144234f;
      sa[90] = 0.19814615f;
      sa[91] = 0.33290955f;
      sa[92] = 1.3358229f;
      sa[93] = 2.0718505f;
      sa[94] = 1.7314316f;
      sa[95] = 0.7506883f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_16x16x6_Tanh_09 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_16x16x6_Tanh_09_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_16x16x6_Tanh_09_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_09_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_nn_16x16x6_Tanh_09_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_09_ColInfo_13_0 implements java.io.Serializable {
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

