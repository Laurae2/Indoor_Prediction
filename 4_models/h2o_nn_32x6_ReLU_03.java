/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:41:29.308+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_32x6_ReLU_03

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_32x6_ReLU_03 > h2o_nn_32x6_ReLU_03.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_32x6_ReLU_03.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_32x6_ReLU_03", algorithm="deeplearning")
public class h2o_nn_32x6_ReLU_03 extends GenModel {
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
  public static final int[] NEURONS = {13,32,5};
    // Thread-local storage for neuron activation values.
    final double[][] ACTIVATION = new double[][] {
      /* Input */ h2o_nn_32x6_ReLU_03_Activation_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_03_Activation_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_03_Activation_2.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_32x6_ReLU_03_Bias_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_03_Bias_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_03_Bias_2.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_32x6_ReLU_03_Weight_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_03_Weight_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_03_Weight_2.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_32x6_ReLU_03.VALUES;
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
    /* Label */ h2o_nn_32x6_ReLU_03_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.27956989247311825,0.13978494623655913,0.15053763440860216,0.27956989247311825,0.15053763440860216};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_32x6_ReLU_03() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-8218228268914694400L); }

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
class h2o_nn_32x6_ReLU_03_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_32x6_ReLU_03_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_03_Activation_0_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_03_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_ReLU_03_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_03_Activation_1_0 implements java.io.Serializable {
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
      sa[16] = 0.0;
      sa[17] = 0.0;
      sa[18] = 0.0;
      sa[19] = 0.0;
      sa[20] = 0.0;
      sa[21] = 0.0;
      sa[22] = 0.0;
      sa[23] = 0.0;
      sa[24] = 0.0;
      sa[25] = 0.0;
      sa[26] = 0.0;
      sa[27] = 0.0;
      sa[28] = 0.0;
      sa[29] = 0.0;
      sa[30] = 0.0;
      sa[31] = 0.0;
    }
  }
}
// Neuron activation values for Softmax layer
class h2o_nn_32x6_ReLU_03_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_32x6_ReLU_03_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_03_Activation_2_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_03_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Rectifier layer
class h2o_nn_32x6_ReLU_03_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_ReLU_03_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_03_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.5355410628251482;
      sa[1] = 0.5089781864537913;
      sa[2] = 0.4946784516933136;
      sa[3] = 0.5048243478644339;
      sa[4] = 0.5855836815558527;
      sa[5] = 0.5109786674354063;
      sa[6] = 0.4521973119732199;
      sa[7] = 0.4670763189563527;
      sa[8] = 0.46746201578310886;
      sa[9] = 0.5406651198086174;
      sa[10] = 0.4548845818894529;
      sa[11] = 0.39978271183166225;
      sa[12] = 0.4878639927967717;
      sa[13] = 0.5176581058620464;
      sa[14] = 0.46712356813807177;
      sa[15] = 0.532558890069622;
      sa[16] = 0.5371669151020659;
      sa[17] = 0.5369175714281095;
      sa[18] = 0.513782894045927;
      sa[19] = 0.4606820811262086;
      sa[20] = 0.4684656871511135;
      sa[21] = 0.47668469973391925;
      sa[22] = 0.5559864491130179;
      sa[23] = 0.5385234569532835;
      sa[24] = 0.47792654560019815;
      sa[25] = 0.45725093735814537;
      sa[26] = 0.48018456920219416;
      sa[27] = 0.38669123879922884;
      sa[28] = 0.4892991732288845;
      sa[29] = 0.5080544441996273;
      sa[30] = 0.5121802531747135;
      sa[31] = 0.5318492461981296;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_32x6_ReLU_03_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_32x6_ReLU_03_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_03_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.0063429073078673185;
      sa[1] = -0.006958961212796116;
      sa[2] = 0.022398291055879576;
      sa[3] = -0.059023239838389266;
      sa[4] = -0.0035045935820372438;
    }
  }
}
class h2o_nn_32x6_ReLU_03_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Rectifier layer
class h2o_nn_32x6_ReLU_03_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[416];
  static {
    h2o_nn_32x6_ReLU_03_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_03_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.042400297f;
      sa[1] = 0.2581547f;
      sa[2] = 0.24168386f;
      sa[3] = 0.16988999f;
      sa[4] = -0.22593507f;
      sa[5] = -0.2077216f;
      sa[6] = 0.050956126f;
      sa[7] = 0.2887804f;
      sa[8] = 0.13717878f;
      sa[9] = -0.17592679f;
      sa[10] = -0.26898345f;
      sa[11] = -0.20829053f;
      sa[12] = -0.020814536f;
      sa[13] = -0.2793572f;
      sa[14] = -0.2736631f;
      sa[15] = -0.045754515f;
      sa[16] = 0.02930489f;
      sa[17] = 0.08680987f;
      sa[18] = -0.19687909f;
      sa[19] = 0.23624428f;
      sa[20] = 0.27109697f;
      sa[21] = -0.31538907f;
      sa[22] = 0.2905808f;
      sa[23] = -0.13590504f;
      sa[24] = 0.2091145f;
      sa[25] = 0.4026252f;
      sa[26] = -0.3421735f;
      sa[27] = 0.3943663f;
      sa[28] = 0.30379486f;
      sa[29] = -0.21160679f;
      sa[30] = -0.10740351f;
      sa[31] = 0.073058784f;
      sa[32] = 0.27771544f;
      sa[33] = -0.34180236f;
      sa[34] = 0.09416705f;
      sa[35] = -0.1547705f;
      sa[36] = 0.14693f;
      sa[37] = 0.29222775f;
      sa[38] = -0.11558256f;
      sa[39] = 0.23258139f;
      sa[40] = 0.04323466f;
      sa[41] = -0.08648103f;
      sa[42] = -0.08412847f;
      sa[43] = 0.1246502f;
      sa[44] = 0.20958777f;
      sa[45] = -0.26550752f;
      sa[46] = 0.036730465f;
      sa[47] = 0.024429346f;
      sa[48] = 0.21094416f;
      sa[49] = 0.42435762f;
      sa[50] = 0.18287128f;
      sa[51] = -0.29338694f;
      sa[52] = -0.06884029f;
      sa[53] = 0.16833025f;
      sa[54] = 0.30679804f;
      sa[55] = 0.08412598f;
      sa[56] = -0.13460845f;
      sa[57] = -0.42283973f;
      sa[58] = 0.45996f;
      sa[59] = -0.22204782f;
      sa[60] = -0.3420008f;
      sa[61] = 0.14952059f;
      sa[62] = 0.21708165f;
      sa[63] = -0.011340447f;
      sa[64] = 0.054805666f;
      sa[65] = 0.17376702f;
      sa[66] = -0.04868868f;
      sa[67] = 0.13133615f;
      sa[68] = -0.19455838f;
      sa[69] = 0.13885117f;
      sa[70] = -0.068176545f;
      sa[71] = 0.27821237f;
      sa[72] = -0.16440223f;
      sa[73] = 0.27472088f;
      sa[74] = -0.18089661f;
      sa[75] = 0.14226207f;
      sa[76] = -0.14830355f;
      sa[77] = -0.04154859f;
      sa[78] = -0.06871295f;
      sa[79] = 0.15284617f;
      sa[80] = -0.28949428f;
      sa[81] = -0.25964192f;
      sa[82] = -0.13583861f;
      sa[83] = 0.28417584f;
      sa[84] = -0.17854507f;
      sa[85] = -0.21083374f;
      sa[86] = 0.20751116f;
      sa[87] = 0.36532992f;
      sa[88] = 0.1855069f;
      sa[89] = 0.10433839f;
      sa[90] = -0.06512906f;
      sa[91] = 0.15967073f;
      sa[92] = 0.21475303f;
      sa[93] = -0.24390854f;
      sa[94] = -0.18185325f;
      sa[95] = -0.24033085f;
      sa[96] = 0.14563924f;
      sa[97] = -0.028947452f;
      sa[98] = 0.26416966f;
      sa[99] = -0.12376879f;
      sa[100] = -0.037049804f;
      sa[101] = -0.24540755f;
      sa[102] = -0.2630577f;
      sa[103] = -0.084759384f;
      sa[104] = 0.045201644f;
      sa[105] = -0.16541971f;
      sa[106] = -0.31701955f;
      sa[107] = 0.1476008f;
      sa[108] = 0.21671002f;
      sa[109] = -0.26003563f;
      sa[110] = -0.21911515f;
      sa[111] = 0.17394151f;
      sa[112] = 0.17244157f;
      sa[113] = 0.006934201f;
      sa[114] = -0.09484272f;
      sa[115] = -0.11028116f;
      sa[116] = -0.1299979f;
      sa[117] = -0.047517657f;
      sa[118] = -0.3424264f;
      sa[119] = -0.16674803f;
      sa[120] = 0.23514545f;
      sa[121] = 0.23283343f;
      sa[122] = 0.35740373f;
      sa[123] = 0.4060407f;
      sa[124] = 0.30727628f;
      sa[125] = 0.30562925f;
      sa[126] = -0.29687637f;
      sa[127] = -0.33153063f;
      sa[128] = 0.08604123f;
      sa[129] = 0.09144359f;
      sa[130] = -0.17666225f;
      sa[131] = 0.37685648f;
      sa[132] = 0.18545863f;
      sa[133] = -0.2941323f;
      sa[134] = 0.08836407f;
      sa[135] = -0.2737368f;
      sa[136] = -0.21100922f;
      sa[137] = -0.037300125f;
      sa[138] = 0.114853404f;
      sa[139] = -0.17025854f;
      sa[140] = -0.15201609f;
      sa[141] = -0.41168734f;
      sa[142] = 0.3334718f;
      sa[143] = -0.34791976f;
      sa[144] = 0.2513862f;
      sa[145] = -0.24396384f;
      sa[146] = -0.41364887f;
      sa[147] = 0.14009045f;
      sa[148] = 0.2845057f;
      sa[149] = 0.29953802f;
      sa[150] = 0.32007664f;
      sa[151] = -0.2832267f;
      sa[152] = 0.048799805f;
      sa[153] = 0.3596167f;
      sa[154] = 0.16563053f;
      sa[155] = -0.075516984f;
      sa[156] = 0.10831996f;
      sa[157] = -0.17815982f;
      sa[158] = 0.12122859f;
      sa[159] = 0.09902166f;
      sa[160] = 0.02026709f;
      sa[161] = 0.28271064f;
      sa[162] = -0.11575017f;
      sa[163] = -0.09151903f;
      sa[164] = 0.25299194f;
      sa[165] = -0.015385603f;
      sa[166] = -0.15427424f;
      sa[167] = -0.15355659f;
      sa[168] = 0.18146151f;
      sa[169] = 0.3376261f;
      sa[170] = -0.026934383f;
      sa[171] = -0.024231205f;
      sa[172] = -0.09666733f;
      sa[173] = -0.21802302f;
      sa[174] = -0.002966871f;
      sa[175] = -0.18748426f;
      sa[176] = -0.062463593f;
      sa[177] = -0.058537547f;
      sa[178] = -0.04092564f;
      sa[179] = 0.057498757f;
      sa[180] = -0.13464092f;
      sa[181] = -0.32361045f;
      sa[182] = -0.12946603f;
      sa[183] = 0.098990664f;
      sa[184] = 0.30292106f;
      sa[185] = 0.21678163f;
      sa[186] = -0.32573652f;
      sa[187] = 0.0845323f;
      sa[188] = -0.13876413f;
      sa[189] = 0.26886812f;
      sa[190] = 0.07080325f;
      sa[191] = -0.06737411f;
      sa[192] = -0.06898979f;
      sa[193] = 0.24972942f;
      sa[194] = 0.19176054f;
      sa[195] = 0.29618415f;
      sa[196] = 0.33493438f;
      sa[197] = 0.0129021155f;
      sa[198] = 0.11566478f;
      sa[199] = -0.071725726f;
      sa[200] = 9.792382E-4f;
      sa[201] = -0.3860641f;
      sa[202] = 0.14973393f;
      sa[203] = -0.05968474f;
      sa[204] = -0.17987685f;
      sa[205] = 0.17349528f;
      sa[206] = 0.3523235f;
      sa[207] = 0.0319115f;
      sa[208] = -0.31368333f;
      sa[209] = -0.15340592f;
      sa[210] = -0.3276222f;
      sa[211] = -0.35672167f;
      sa[212] = 0.21418548f;
      sa[213] = -0.28505212f;
      sa[214] = -0.048382077f;
      sa[215] = 0.08625143f;
      sa[216] = 0.11500912f;
      sa[217] = 0.2469909f;
      sa[218] = -0.2223233f;
      sa[219] = 0.31720516f;
      sa[220] = -0.33041042f;
      sa[221] = -0.0045062634f;
      sa[222] = 0.2703834f;
      sa[223] = 0.10317036f;
      sa[224] = 0.1366514f;
      sa[225] = 0.30916703f;
      sa[226] = -0.24231805f;
      sa[227] = 0.49719068f;
      sa[228] = 0.14752509f;
      sa[229] = 0.08903534f;
      sa[230] = 0.27781752f;
      sa[231] = -0.35768235f;
      sa[232] = -0.41908285f;
      sa[233] = 0.15408328f;
      sa[234] = 0.16125731f;
      sa[235] = -0.28662562f;
      sa[236] = -0.01848909f;
      sa[237] = -0.1575198f;
      sa[238] = -0.16459264f;
      sa[239] = 0.0107320035f;
      sa[240] = 0.10637444f;
      sa[241] = -0.2539192f;
      sa[242] = -0.12535799f;
      sa[243] = -0.3182427f;
      sa[244] = -0.23370256f;
      sa[245] = -0.1908817f;
      sa[246] = -0.3748958f;
      sa[247] = -0.031898655f;
      sa[248] = 0.33694637f;
      sa[249] = 0.26844588f;
      sa[250] = -0.24882099f;
      sa[251] = -0.109620936f;
      sa[252] = -0.32181403f;
      sa[253] = -0.45673236f;
      sa[254] = -0.29099262f;
      sa[255] = 0.2528058f;
      sa[256] = -0.21928395f;
      sa[257] = 0.42117792f;
      sa[258] = -0.30616826f;
      sa[259] = -0.022456102f;
      sa[260] = -0.28471485f;
      sa[261] = -0.09955991f;
      sa[262] = -0.19260389f;
      sa[263] = 0.19966303f;
      sa[264] = -0.30592254f;
      sa[265] = 0.2288893f;
      sa[266] = -0.5304726f;
      sa[267] = -0.28154123f;
      sa[268] = 0.13855071f;
      sa[269] = -0.11060075f;
      sa[270] = -0.1341494f;
      sa[271] = 0.28014323f;
      sa[272] = -0.15286574f;
      sa[273] = -0.0894464f;
      sa[274] = 0.12674017f;
      sa[275] = -0.03921716f;
      sa[276] = 0.089898095f;
      sa[277] = -0.24856608f;
      sa[278] = -0.10451444f;
      sa[279] = 0.052818988f;
      sa[280] = -0.20425843f;
      sa[281] = 0.3602476f;
      sa[282] = -0.114024535f;
      sa[283] = 0.019879252f;
      sa[284] = 0.08902248f;
      sa[285] = -0.064658076f;
      sa[286] = 0.2810881f;
      sa[287] = 0.1765455f;
      sa[288] = -0.18456337f;
      sa[289] = -0.049109418f;
      sa[290] = 0.008532259f;
      sa[291] = -0.13573667f;
      sa[292] = 0.19728361f;
      sa[293] = -0.097797796f;
      sa[294] = 0.06846103f;
      sa[295] = 0.075674556f;
      sa[296] = 0.3231639f;
      sa[297] = 0.22497134f;
      sa[298] = -0.1718453f;
      sa[299] = -0.06437411f;
      sa[300] = 0.0226419f;
      sa[301] = -0.007606985f;
      sa[302] = 0.1987573f;
      sa[303] = 0.032262523f;
      sa[304] = -0.26079547f;
      sa[305] = -0.07559511f;
      sa[306] = 0.24742955f;
      sa[307] = -0.21992218f;
      sa[308] = 0.13679871f;
      sa[309] = 0.091204196f;
      sa[310] = 0.40344253f;
      sa[311] = 0.17394748f;
      sa[312] = -0.22079323f;
      sa[313] = -0.21635042f;
      sa[314] = -0.22016205f;
      sa[315] = 0.0033255343f;
      sa[316] = 0.0080320565f;
      sa[317] = -0.13014545f;
      sa[318] = 0.121119425f;
      sa[319] = 0.31115606f;
      sa[320] = -0.04779167f;
      sa[321] = 0.12759048f;
      sa[322] = 0.31281924f;
      sa[323] = -0.26792508f;
      sa[324] = 0.14195623f;
      sa[325] = 0.3152953f;
      sa[326] = -0.31389457f;
      sa[327] = -0.019636398f;
      sa[328] = -0.1967825f;
      sa[329] = 0.13765083f;
      sa[330] = -0.104195535f;
      sa[331] = 0.00702539f;
      sa[332] = -0.31599262f;
      sa[333] = 0.14829586f;
      sa[334] = -0.043224845f;
      sa[335] = 0.13304079f;
      sa[336] = -0.19045126f;
      sa[337] = -0.15222701f;
      sa[338] = 0.35467884f;
      sa[339] = -0.26673368f;
      sa[340] = -0.39010823f;
      sa[341] = -0.28591487f;
      sa[342] = 0.23633298f;
      sa[343] = -0.1180593f;
      sa[344] = -0.23642066f;
      sa[345] = 0.23693086f;
      sa[346] = 0.26689273f;
      sa[347] = -0.22212203f;
      sa[348] = 0.41085342f;
      sa[349] = -0.33926013f;
      sa[350] = -0.17343456f;
      sa[351] = 0.09687256f;
      sa[352] = 0.2758291f;
      sa[353] = -0.049159307f;
      sa[354] = 0.13577147f;
      sa[355] = 0.13048935f;
      sa[356] = -0.19078647f;
      sa[357] = 0.27596474f;
      sa[358] = -0.36243975f;
      sa[359] = -0.0039594746f;
      sa[360] = 0.13957971f;
      sa[361] = -0.108143024f;
      sa[362] = -0.17862909f;
      sa[363] = 0.084599786f;
      sa[364] = -0.12502979f;
      sa[365] = -0.17288762f;
      sa[366] = 0.017987961f;
      sa[367] = -0.2611376f;
      sa[368] = -0.13971506f;
      sa[369] = 0.27713245f;
      sa[370] = -0.2921513f;
      sa[371] = 0.14697663f;
      sa[372] = -0.0183245f;
      sa[373] = 0.3221137f;
      sa[374] = -0.12857825f;
      sa[375] = -0.30582085f;
      sa[376] = -0.3522745f;
      sa[377] = -0.2623554f;
      sa[378] = 0.37171164f;
      sa[379] = 0.07596553f;
      sa[380] = -0.1831543f;
      sa[381] = -0.17424178f;
      sa[382] = 0.024558546f;
      sa[383] = -0.05846143f;
      sa[384] = -0.17489171f;
      sa[385] = 0.10715457f;
      sa[386] = -0.2592293f;
      sa[387] = -0.2836931f;
      sa[388] = -0.1009773f;
      sa[389] = 0.10640711f;
      sa[390] = 0.016325189f;
      sa[391] = -0.13696752f;
      sa[392] = 0.30017897f;
      sa[393] = 0.19567958f;
      sa[394] = -0.12801626f;
      sa[395] = 0.401085f;
      sa[396] = 0.2684769f;
      sa[397] = -0.3103264f;
      sa[398] = 0.15794161f;
      sa[399] = 0.18000592f;
      sa[400] = 0.1800444f;
      sa[401] = 0.39393476f;
      sa[402] = -0.2399888f;
      sa[403] = 0.113763675f;
      sa[404] = 0.34095335f;
      sa[405] = -0.2681348f;
      sa[406] = -0.062235147f;
      sa[407] = 0.2097642f;
      sa[408] = -0.22720794f;
      sa[409] = -0.4043367f;
      sa[410] = -0.10251331f;
      sa[411] = 0.22357145f;
      sa[412] = -0.14407742f;
      sa[413] = 0.11440935f;
      sa[414] = 0.15569654f;
      sa[415] = 0.35970178f;
    }
  }
}
// Neuron weights connecting Rectifier and Softmax layer
class h2o_nn_32x6_ReLU_03_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[160];
  static {
    h2o_nn_32x6_ReLU_03_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_03_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 1.2637299f;
      sa[1] = -0.13610291f;
      sa[2] = -0.5086021f;
      sa[3] = -0.892754f;
      sa[4] = 0.060915194f;
      sa[5] = 0.18940876f;
      sa[6] = 1.2300535f;
      sa[7] = -0.64750266f;
      sa[8] = -0.92094356f;
      sa[9] = 0.6524216f;
      sa[10] = -0.25468212f;
      sa[11] = 0.45276213f;
      sa[12] = 0.20517857f;
      sa[13] = 0.11960292f;
      sa[14] = -1.2145619f;
      sa[15] = 1.1262332f;
      sa[16] = 1.6432263f;
      sa[17] = -0.5727677f;
      sa[18] = 0.07118861f;
      sa[19] = -0.52055275f;
      sa[20] = 1.2078246f;
      sa[21] = 0.025139075f;
      sa[22] = -1.0145305f;
      sa[23] = 0.7512449f;
      sa[24] = -1.3876718f;
      sa[25] = 0.43205777f;
      sa[26] = -0.23712274f;
      sa[27] = 1.4355505f;
      sa[28] = -1.3875512f;
      sa[29] = -0.44211322f;
      sa[30] = 0.46668184f;
      sa[31] = -0.8269015f;
      sa[32] = -1.1746081f;
      sa[33] = -1.4922397f;
      sa[34] = 0.0344618f;
      sa[35] = 1.3880756f;
      sa[36] = -0.3783145f;
      sa[37] = -0.10639732f;
      sa[38] = 0.1704438f;
      sa[39] = 0.8022494f;
      sa[40] = -0.7403893f;
      sa[41] = -1.3882495f;
      sa[42] = -1.0479666f;
      sa[43] = -1.3935493f;
      sa[44] = 0.0021604872f;
      sa[45] = -1.3355416f;
      sa[46] = 1.2211998f;
      sa[47] = 1.3818421f;
      sa[48] = -1.324575f;
      sa[49] = -1.0515897f;
      sa[50] = 0.3702515f;
      sa[51] = -0.3090912f;
      sa[52] = 0.82393414f;
      sa[53] = 0.8883166f;
      sa[54] = -0.23150884f;
      sa[55] = 1.1695045f;
      sa[56] = -1.0577527f;
      sa[57] = -1.1251371f;
      sa[58] = 0.18179768f;
      sa[59] = -0.21917602f;
      sa[60] = 1.3416014f;
      sa[61] = -0.60401106f;
      sa[62] = -0.36384788f;
      sa[63] = 0.750475f;
      sa[64] = 1.4852501f;
      sa[65] = -0.6297961f;
      sa[66] = 0.57634044f;
      sa[67] = 1.5359834f;
      sa[68] = 0.7959688f;
      sa[69] = -0.96559834f;
      sa[70] = -1.6421129f;
      sa[71] = -0.43701065f;
      sa[72] = -1.2102108f;
      sa[73] = 1.2053728f;
      sa[74] = 1.3368826f;
      sa[75] = -0.12224874f;
      sa[76] = 0.88966036f;
      sa[77] = 0.5215147f;
      sa[78] = -1.3240826f;
      sa[79] = 0.91631395f;
      sa[80] = -0.79206353f;
      sa[81] = 0.33515263f;
      sa[82] = 0.8420973f;
      sa[83] = -0.5956759f;
      sa[84] = -1.6883442f;
      sa[85] = -1.257651f;
      sa[86] = 0.24889715f;
      sa[87] = 1.2156435f;
      sa[88] = -1.4975835f;
      sa[89] = -0.46097282f;
      sa[90] = -1.0249789f;
      sa[91] = 1.5860642f;
      sa[92] = -0.33852854f;
      sa[93] = 0.25716692f;
      sa[94] = -1.531122f;
      sa[95] = -0.25807297f;
      sa[96] = -1.1148653f;
      sa[97] = 0.1670819f;
      sa[98] = -0.11101877f;
      sa[99] = 0.2371186f;
      sa[100] = -1.2932932f;
      sa[101] = -0.45549276f;
      sa[102] = 1.3048667f;
      sa[103] = 0.04082806f;
      sa[104] = -0.2672147f;
      sa[105] = -0.570409f;
      sa[106] = 1.5105737f;
      sa[107] = 1.0908688f;
      sa[108] = 0.75021464f;
      sa[109] = 0.04884344f;
      sa[110] = 0.66902995f;
      sa[111] = -0.8211364f;
      sa[112] = -1.2256595f;
      sa[113] = 0.16732706f;
      sa[114] = 0.15534548f;
      sa[115] = -0.32452303f;
      sa[116] = 1.1259521f;
      sa[117] = 8.160642E-4f;
      sa[118] = -1.3582834f;
      sa[119] = -0.84313023f;
      sa[120] = -0.33138502f;
      sa[121] = 0.9857368f;
      sa[122] = -0.14893928f;
      sa[123] = 0.35687315f;
      sa[124] = -0.7293762f;
      sa[125] = 0.7991212f;
      sa[126] = -1.0454392f;
      sa[127] = 0.52215177f;
      sa[128] = -0.90745884f;
      sa[129] = 0.78554595f;
      sa[130] = -0.411229f;
      sa[131] = 0.86964333f;
      sa[132] = 0.46483225f;
      sa[133] = 0.62600607f;
      sa[134] = 0.6627575f;
      sa[135] = -1.1259001f;
      sa[136] = 0.5285632f;
      sa[137] = -0.57456017f;
      sa[138] = -0.08468984f;
      sa[139] = 1.1593754f;
      sa[140] = -0.44867945f;
      sa[141] = -0.07277927f;
      sa[142] = 0.2630808f;
      sa[143] = -0.7886284f;
      sa[144] = 0.9239005f;
      sa[145] = -0.7546586f;
      sa[146] = 0.025291173f;
      sa[147] = -0.2821402f;
      sa[148] = -0.8867525f;
      sa[149] = 1.0928746f;
      sa[150] = -1.0739058f;
      sa[151] = -1.5354639f;
      sa[152] = 1.6009363f;
      sa[153] = -0.5686869f;
      sa[154] = 1.3440295f;
      sa[155] = -1.5222828f;
      sa[156] = -1.4544524f;
      sa[157] = -1.154183f;
      sa[158] = -0.22909634f;
      sa[159] = -0.5772167f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_32x6_ReLU_03 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_32x6_ReLU_03_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_32x6_ReLU_03_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_03_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    h2o_nn_32x6_ReLU_03_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_03_ColInfo_13_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "4";
      sa[3] = "5";
      sa[4] = "6";
    }
  }
}

