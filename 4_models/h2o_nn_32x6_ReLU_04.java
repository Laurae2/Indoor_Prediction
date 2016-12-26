/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:41:40.647+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_32x6_ReLU_04

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_32x6_ReLU_04 > h2o_nn_32x6_ReLU_04.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_32x6_ReLU_04.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_32x6_ReLU_04", algorithm="deeplearning")
public class h2o_nn_32x6_ReLU_04 extends GenModel {
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
  public static final int[] NEURONS = {13,32,6};
    // Thread-local storage for neuron activation values.
    final double[][] ACTIVATION = new double[][] {
      /* Input */ h2o_nn_32x6_ReLU_04_Activation_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_04_Activation_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_04_Activation_2.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_32x6_ReLU_04_Bias_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_04_Bias_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_04_Bias_2.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_32x6_ReLU_04_Weight_0.VALUES,
      /* Rectifier */ h2o_nn_32x6_ReLU_04_Weight_1.VALUES,
      /* Softmax */ h2o_nn_32x6_ReLU_04_Weight_2.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_32x6_ReLU_04.VALUES;
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
    /* Label */ h2o_nn_32x6_ReLU_04_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.24528301886792453,0.12264150943396226,0.12264150943396226,0.1320754716981132,0.24528301886792453,0.1320754716981132};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_32x6_ReLU_04() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-4227631986151307800L); }

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
class h2o_nn_32x6_ReLU_04_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_32x6_ReLU_04_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_04_Activation_0_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_04_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_ReLU_04_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_04_Activation_1_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_04_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_32x6_ReLU_04_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_04_Activation_2_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_04_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Rectifier layer
class h2o_nn_32x6_ReLU_04_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_ReLU_04_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_04_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.48939777223643327;
      sa[1] = 0.5683474613662347;
      sa[2] = 0.4057593824676896;
      sa[3] = 0.48850494265020705;
      sa[4] = 0.4631673712930285;
      sa[5] = 0.5159393323231677;
      sa[6] = 0.5243037506987425;
      sa[7] = 0.5255127286265813;
      sa[8] = 0.496200250672023;
      sa[9] = 0.5392810505086512;
      sa[10] = 0.6111872051773088;
      sa[11] = 0.5152897616470016;
      sa[12] = 0.4736163413249607;
      sa[13] = 0.4764165087827885;
      sa[14] = 0.4889904052014853;
      sa[15] = 0.543465107916204;
      sa[16] = 0.5307592463998779;
      sa[17] = 0.5790480467585482;
      sa[18] = 0.5734845834637654;
      sa[19] = 0.5141612919027402;
      sa[20] = 0.553400326959169;
      sa[21] = 0.5082650901377525;
      sa[22] = 0.4844090622337606;
      sa[23] = 0.5545318092209709;
      sa[24] = 0.5390301029047037;
      sa[25] = 0.5567924301901271;
      sa[26] = 0.48805298451501544;
      sa[27] = 0.24564874559606817;
      sa[28] = 0.4563662777942086;
      sa[29] = 0.5678493628654002;
      sa[30] = 0.5366806792763529;
      sa[31] = 0.6211853895198318;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_32x6_ReLU_04_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_32x6_ReLU_04_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_04_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.002071566772747719;
      sa[1] = -0.03141608622202643;
      sa[2] = -0.014594426349269118;
      sa[3] = 0.00858867763348857;
      sa[4] = -0.019477540338259342;
      sa[5] = -0.043885313831509794;
    }
  }
}
class h2o_nn_32x6_ReLU_04_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Rectifier layer
class h2o_nn_32x6_ReLU_04_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[416];
  static {
    h2o_nn_32x6_ReLU_04_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_04_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.0887748f;
      sa[1] = 0.23414908f;
      sa[2] = 0.17675567f;
      sa[3] = 0.081367046f;
      sa[4] = -0.2905824f;
      sa[5] = -0.12757443f;
      sa[6] = -0.023879904f;
      sa[7] = 0.26223657f;
      sa[8] = 0.16074482f;
      sa[9] = -0.1345483f;
      sa[10] = -0.13166045f;
      sa[11] = -0.08201974f;
      sa[12] = -0.06376503f;
      sa[13] = -0.33181325f;
      sa[14] = -0.103046566f;
      sa[15] = 0.03559418f;
      sa[16] = 0.051962294f;
      sa[17] = -0.03676078f;
      sa[18] = -0.17224929f;
      sa[19] = 0.32409793f;
      sa[20] = 0.26204056f;
      sa[21] = -0.3528587f;
      sa[22] = 0.35403273f;
      sa[23] = -0.46250865f;
      sa[24] = 0.11395079f;
      sa[25] = 0.39999926f;
      sa[26] = -0.30234587f;
      sa[27] = 0.52261513f;
      sa[28] = 0.19932255f;
      sa[29] = -0.14437978f;
      sa[30] = -0.082378715f;
      sa[31] = 0.0879009f;
      sa[32] = 0.23257054f;
      sa[33] = -0.33482784f;
      sa[34] = 0.11221106f;
      sa[35] = -0.16701949f;
      sa[36] = 0.1761899f;
      sa[37] = 0.48444352f;
      sa[38] = -0.17736945f;
      sa[39] = 0.26086375f;
      sa[40] = 0.4379403f;
      sa[41] = -0.07472302f;
      sa[42] = -0.122939326f;
      sa[43] = 0.09603386f;
      sa[44] = 0.2988703f;
      sa[45] = -0.5191771f;
      sa[46] = 0.06644126f;
      sa[47] = 0.036931995f;
      sa[48] = 0.26376823f;
      sa[49] = 0.14220965f;
      sa[50] = 0.26863387f;
      sa[51] = -0.30637458f;
      sa[52] = -0.032710984f;
      sa[53] = 0.30068567f;
      sa[54] = 0.17756268f;
      sa[55] = -0.1575349f;
      sa[56] = -0.45780027f;
      sa[57] = -0.14640813f;
      sa[58] = 0.10629652f;
      sa[59] = -0.24395555f;
      sa[60] = -0.2888507f;
      sa[61] = 0.24564008f;
      sa[62] = 0.1561379f;
      sa[63] = 0.07624609f;
      sa[64] = 0.1667181f;
      sa[65] = 0.092156224f;
      sa[66] = -0.14853454f;
      sa[67] = 0.124841936f;
      sa[68] = -0.23542556f;
      sa[69] = -0.09975535f;
      sa[70] = 0.018120907f;
      sa[71] = 0.22263947f;
      sa[72] = -0.16741098f;
      sa[73] = 0.26894453f;
      sa[74] = -0.12040417f;
      sa[75] = -0.100940526f;
      sa[76] = -0.3697342f;
      sa[77] = 0.15040588f;
      sa[78] = -0.112023905f;
      sa[79] = -0.2017216f;
      sa[80] = -0.25247225f;
      sa[81] = -0.13067704f;
      sa[82] = -0.066426925f;
      sa[83] = 0.07867022f;
      sa[84] = 0.11460114f;
      sa[85] = -0.20060496f;
      sa[86] = 0.18171163f;
      sa[87] = 0.26952243f;
      sa[88] = 0.40284076f;
      sa[89] = -0.03062741f;
      sa[90] = -0.040749922f;
      sa[91] = 0.13809894f;
      sa[92] = 0.24579944f;
      sa[93] = -0.16668431f;
      sa[94] = 0.038940676f;
      sa[95] = 0.02339179f;
      sa[96] = 0.009546231f;
      sa[97] = 0.031033762f;
      sa[98] = 0.27963215f;
      sa[99] = -0.16127297f;
      sa[100] = -0.08190322f;
      sa[101] = -0.27928203f;
      sa[102] = -0.31368628f;
      sa[103] = -0.24066895f;
      sa[104] = 0.054132868f;
      sa[105] = -0.34084642f;
      sa[106] = -0.38017148f;
      sa[107] = 0.16225117f;
      sa[108] = 0.2287259f;
      sa[109] = -0.26828957f;
      sa[110] = -0.15984143f;
      sa[111] = 0.2175522f;
      sa[112] = 0.23977897f;
      sa[113] = -0.057829387f;
      sa[114] = 0.10397517f;
      sa[115] = -0.22195981f;
      sa[116] = 0.085964605f;
      sa[117] = -0.052752737f;
      sa[118] = -0.10939813f;
      sa[119] = -0.1549354f;
      sa[120] = 0.16215426f;
      sa[121] = 0.12749664f;
      sa[122] = 0.4603467f;
      sa[123] = 0.33320248f;
      sa[124] = 0.2693935f;
      sa[125] = 0.29789087f;
      sa[126] = -0.18659128f;
      sa[127] = -0.5996754f;
      sa[128] = 0.13658038f;
      sa[129] = -0.06357799f;
      sa[130] = -0.13445337f;
      sa[131] = 0.6468301f;
      sa[132] = 0.26483074f;
      sa[133] = -0.04245631f;
      sa[134] = 0.31319055f;
      sa[135] = -0.35746485f;
      sa[136] = 0.13306794f;
      sa[137] = -0.015433129f;
      sa[138] = 0.11951001f;
      sa[139] = -0.19118832f;
      sa[140] = 0.042502627f;
      sa[141] = -0.21853755f;
      sa[142] = 0.13939954f;
      sa[143] = -0.3731907f;
      sa[144] = 0.293639f;
      sa[145] = -0.15756129f;
      sa[146] = -0.30338776f;
      sa[147] = 0.037383836f;
      sa[148] = 0.24564615f;
      sa[149] = 0.45718044f;
      sa[150] = 0.3278635f;
      sa[151] = -0.29210708f;
      sa[152] = 0.025385795f;
      sa[153] = 0.24584638f;
      sa[154] = 0.08055729f;
      sa[155] = -0.037491355f;
      sa[156] = 0.17521685f;
      sa[157] = -0.2358339f;
      sa[158] = 0.1020222f;
      sa[159] = 0.22680198f;
      sa[160] = 0.28350118f;
      sa[161] = 0.12006822f;
      sa[162] = 0.09723647f;
      sa[163] = -0.09740116f;
      sa[164] = 0.26326323f;
      sa[165] = -0.15889116f;
      sa[166] = 0.1286965f;
      sa[167] = -0.0024182124f;
      sa[168] = 0.0014696348f;
      sa[169] = 0.3770855f;
      sa[170] = -0.014345715f;
      sa[171] = -0.08099173f;
      sa[172] = -0.10580815f;
      sa[173] = -0.2167166f;
      sa[174] = 0.0027402432f;
      sa[175] = -0.102338515f;
      sa[176] = -0.082531914f;
      sa[177] = -0.042582918f;
      sa[178] = -0.05120985f;
      sa[179] = 0.3494686f;
      sa[180] = 0.010293375f;
      sa[181] = -0.33682182f;
      sa[182] = -0.15409961f;
      sa[183] = 0.036376506f;
      sa[184] = 0.33535355f;
      sa[185] = 0.3050254f;
      sa[186] = -0.23307215f;
      sa[187] = -0.010033354f;
      sa[188] = -0.05308193f;
      sa[189] = 0.2909126f;
      sa[190] = 0.056077108f;
      sa[191] = -0.13068828f;
      sa[192] = -0.003363204f;
      sa[193] = 0.15838873f;
      sa[194] = 0.2972105f;
      sa[195] = 0.36312026f;
      sa[196] = 0.2082934f;
      sa[197] = -0.038379926f;
      sa[198] = 0.0425373f;
      sa[199] = -0.084732234f;
      sa[200] = 0.045755386f;
      sa[201] = -0.5370732f;
      sa[202] = 0.15181507f;
      sa[203] = -0.025446866f;
      sa[204] = -0.11025437f;
      sa[205] = 0.2994883f;
      sa[206] = 0.5028601f;
      sa[207] = -0.11313561f;
      sa[208] = -0.35559204f;
      sa[209] = -0.2179856f;
      sa[210] = -0.34550217f;
      sa[211] = -0.4905594f;
      sa[212] = 0.015067614f;
      sa[213] = -0.14666875f;
      sa[214] = -0.12395504f;
      sa[215] = 0.060232285f;
      sa[216] = 0.12667327f;
      sa[217] = 0.3107249f;
      sa[218] = -0.4897546f;
      sa[219] = 0.2293585f;
      sa[220] = -0.2756612f;
      sa[221] = 0.05285289f;
      sa[222] = 0.38957128f;
      sa[223] = 0.14407836f;
      sa[224] = 0.28184846f;
      sa[225] = 0.48865947f;
      sa[226] = -0.32009166f;
      sa[227] = 0.6527632f;
      sa[228] = 0.14680032f;
      sa[229] = 0.08142732f;
      sa[230] = 0.18491697f;
      sa[231] = -0.2040723f;
      sa[232] = -0.26513132f;
      sa[233] = -0.159731f;
      sa[234] = 0.17242463f;
      sa[235] = 0.02317642f;
      sa[236] = 0.07990296f;
      sa[237] = -0.2751868f;
      sa[238] = -0.29164064f;
      sa[239] = 0.2538735f;
      sa[240] = -0.08932957f;
      sa[241] = -0.23824355f;
      sa[242] = -0.10610089f;
      sa[243] = -0.18341038f;
      sa[244] = -0.5927196f;
      sa[245] = -0.18508534f;
      sa[246] = -0.49595958f;
      sa[247] = -0.058594543f;
      sa[248] = 0.18291515f;
      sa[249] = 0.30173448f;
      sa[250] = -0.17070508f;
      sa[251] = -0.035577666f;
      sa[252] = -0.45455906f;
      sa[253] = -0.2895234f;
      sa[254] = -0.2564824f;
      sa[255] = 0.24302207f;
      sa[256] = -0.29818055f;
      sa[257] = 0.52824503f;
      sa[258] = -0.40924576f;
      sa[259] = 0.12376584f;
      sa[260] = -0.35673055f;
      sa[261] = -0.40498385f;
      sa[262] = -0.08742954f;
      sa[263] = 0.3302694f;
      sa[264] = -0.18828617f;
      sa[265] = -0.018545726f;
      sa[266] = -0.15190138f;
      sa[267] = -0.27579066f;
      sa[268] = 0.072939105f;
      sa[269] = -0.1958359f;
      sa[270] = -0.10425166f;
      sa[271] = 0.1229164f;
      sa[272] = -0.20037833f;
      sa[273] = -0.17827155f;
      sa[274] = 0.064275f;
      sa[275] = -0.0032117793f;
      sa[276] = 0.06519634f;
      sa[277] = -0.3665219f;
      sa[278] = -0.08095248f;
      sa[279] = 0.011752113f;
      sa[280] = -0.18661585f;
      sa[281] = 0.34300074f;
      sa[282] = -0.10711727f;
      sa[283] = -0.17988034f;
      sa[284] = -0.08905291f;
      sa[285] = 0.05136298f;
      sa[286] = 0.3714025f;
      sa[287] = 0.1916521f;
      sa[288] = -0.27551624f;
      sa[289] = -0.16937129f;
      sa[290] = 0.08776424f;
      sa[291] = -0.017720979f;
      sa[292] = -0.07676906f;
      sa[293] = -0.09715567f;
      sa[294] = 0.13131975f;
      sa[295] = 0.115989015f;
      sa[296] = 0.46984094f;
      sa[297] = 0.42430922f;
      sa[298] = -0.12158464f;
      sa[299] = -0.006887802f;
      sa[300] = 0.034140028f;
      sa[301] = -0.013490687f;
      sa[302] = 0.15054747f;
      sa[303] = 0.057287723f;
      sa[304] = -0.24009907f;
      sa[305] = -0.20184246f;
      sa[306] = 0.24965738f;
      sa[307] = -0.20182633f;
      sa[308] = 0.20459145f;
      sa[309] = 0.09421564f;
      sa[310] = 0.5581618f;
      sa[311] = -0.05446624f;
      sa[312] = -0.18212236f;
      sa[313] = -0.2589849f;
      sa[314] = -0.2671748f;
      sa[315] = -0.032793906f;
      sa[316] = -0.06162814f;
      sa[317] = -0.04317422f;
      sa[318] = 0.038814723f;
      sa[319] = 0.36490574f;
      sa[320] = 0.0412257f;
      sa[321] = 0.099716686f;
      sa[322] = 0.43246675f;
      sa[323] = -0.3389384f;
      sa[324] = 0.41635838f;
      sa[325] = 0.31833088f;
      sa[326] = -0.52847385f;
      sa[327] = 0.061681565f;
      sa[328] = 0.0105444705f;
      sa[329] = 0.35404122f;
      sa[330] = -0.31164604f;
      sa[331] = 0.3841385f;
      sa[332] = -0.30951443f;
      sa[333] = 0.13355175f;
      sa[334] = -0.17167014f;
      sa[335] = 0.44985822f;
      sa[336] = -0.2539909f;
      sa[337] = -0.23754318f;
      sa[338] = 0.3026468f;
      sa[339] = -0.2748831f;
      sa[340] = -0.387835f;
      sa[341] = -0.34363687f;
      sa[342] = 0.085745536f;
      sa[343] = -0.019052586f;
      sa[344] = -0.37922868f;
      sa[345] = 0.25111675f;
      sa[346] = 0.2702428f;
      sa[347] = -0.19981967f;
      sa[348] = 0.22957067f;
      sa[349] = -0.48086601f;
      sa[350] = 0.107730515f;
      sa[351] = 0.051426403f;
      sa[352] = 0.30995592f;
      sa[353] = -0.056228973f;
      sa[354] = 0.19088393f;
      sa[355] = 0.23333247f;
      sa[356] = -0.26912355f;
      sa[357] = 0.38266522f;
      sa[358] = -0.4238667f;
      sa[359] = -0.08187534f;
      sa[360] = 0.08291897f;
      sa[361] = -0.08596372f;
      sa[362] = -0.15436275f;
      sa[363] = 0.12043637f;
      sa[364] = -0.08204161f;
      sa[365] = -0.20692618f;
      sa[366] = 0.002777564f;
      sa[367] = -0.17248851f;
      sa[368] = 0.011191094f;
      sa[369] = 0.21212573f;
      sa[370] = -0.388752f;
      sa[371] = 0.17112796f;
      sa[372] = -0.014419524f;
      sa[373] = 0.27453518f;
      sa[374] = 0.2761183f;
      sa[375] = -0.08216426f;
      sa[376] = -0.39566424f;
      sa[377] = -0.2587955f;
      sa[378] = 0.48207915f;
      sa[379] = 0.18271618f;
      sa[380] = 0.078740515f;
      sa[381] = 0.123707876f;
      sa[382] = -0.16337912f;
      sa[383] = 0.2546163f;
      sa[384] = -0.17157543f;
      sa[385] = 0.07666614f;
      sa[386] = -0.32891396f;
      sa[387] = -0.2671545f;
      sa[388] = 0.0459413f;
      sa[389] = -0.17421906f;
      sa[390] = -0.06973606f;
      sa[391] = -0.32698825f;
      sa[392] = 0.31386894f;
      sa[393] = 0.09569728f;
      sa[394] = -0.2898055f;
      sa[395] = 0.46945387f;
      sa[396] = 0.2381444f;
      sa[397] = -0.32436547f;
      sa[398] = 0.1497898f;
      sa[399] = 0.2245221f;
      sa[400] = -0.114762835f;
      sa[401] = 0.19794844f;
      sa[402] = -0.16866474f;
      sa[403] = 0.12509362f;
      sa[404] = 0.46684024f;
      sa[405] = -0.17801867f;
      sa[406] = 0.09581817f;
      sa[407] = 0.35057977f;
      sa[408] = -0.3680911f;
      sa[409] = -0.34275824f;
      sa[410] = -0.05827864f;
      sa[411] = 0.2162732f;
      sa[412] = -0.19569865f;
      sa[413] = 0.09214592f;
      sa[414] = 0.13581291f;
      sa[415] = 0.22031648f;
    }
  }
}
// Neuron weights connecting Rectifier and Softmax layer
class h2o_nn_32x6_ReLU_04_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[192];
  static {
    h2o_nn_32x6_ReLU_04_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_04_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 1.2366102f;
      sa[1] = -0.12765704f;
      sa[2] = -0.52652633f;
      sa[3] = -0.8840338f;
      sa[4] = 0.07531189f;
      sa[5] = 0.21004675f;
      sa[6] = 1.2114816f;
      sa[7] = -0.6382583f;
      sa[8] = -0.936624f;
      sa[9] = 0.6778311f;
      sa[10] = -0.38353205f;
      sa[11] = 0.430525f;
      sa[12] = 0.19605412f;
      sa[13] = 0.12066488f;
      sa[14] = -1.2286328f;
      sa[15] = 1.0536593f;
      sa[16] = 1.6377424f;
      sa[17] = -0.59663904f;
      sa[18] = 0.11653479f;
      sa[19] = -0.55487645f;
      sa[20] = 1.2006657f;
      sa[21] = 0.031788025f;
      sa[22] = -1.0190352f;
      sa[23] = 0.6810096f;
      sa[24] = -1.3712729f;
      sa[25] = 0.44050092f;
      sa[26] = -0.22623418f;
      sa[27] = 1.3478961f;
      sa[28] = -1.3392701f;
      sa[29] = -0.47991875f;
      sa[30] = 0.48081627f;
      sa[31] = -1.1925342f;
      sa[32] = -1.2024355f;
      sa[33] = -1.4969844f;
      sa[34] = 9.6019695E-4f;
      sa[35] = 1.3767607f;
      sa[36] = -0.46739602f;
      sa[37] = -0.18673503f;
      sa[38] = 0.14859566f;
      sa[39] = 0.7125309f;
      sa[40] = -0.6916565f;
      sa[41] = -1.3963097f;
      sa[42] = -1.0269483f;
      sa[43] = -1.4152877f;
      sa[44] = 0.023857042f;
      sa[45] = -1.3141246f;
      sa[46] = 1.1815524f;
      sa[47] = 1.4376434f;
      sa[48] = -1.3220477f;
      sa[49] = -1.065162f;
      sa[50] = 0.31372032f;
      sa[51] = -0.29797184f;
      sa[52] = 0.80553645f;
      sa[53] = 0.79062116f;
      sa[54] = -0.2156954f;
      sa[55] = 1.2279189f;
      sa[56] = -1.0626073f;
      sa[57] = -1.0527891f;
      sa[58] = 0.1874947f;
      sa[59] = -0.24408822f;
      sa[60] = 1.3227445f;
      sa[61] = -0.6338619f;
      sa[62] = -0.40785027f;
      sa[63] = 0.8976939f;
      sa[64] = 1.440964f;
      sa[65] = -0.68444014f;
      sa[66] = 0.69120944f;
      sa[67] = 1.6263782f;
      sa[68] = 0.7072108f;
      sa[69] = -1.1225682f;
      sa[70] = -1.5191355f;
      sa[71] = -0.40525922f;
      sa[72] = -1.3332924f;
      sa[73] = 1.1398568f;
      sa[74] = 1.3339566f;
      sa[75] = -0.061853502f;
      sa[76] = 0.83659047f;
      sa[77] = 0.51978314f;
      sa[78] = -1.2686229f;
      sa[79] = 1.0234503f;
      sa[80] = -0.74159145f;
      sa[81] = 0.29116553f;
      sa[82] = 0.8251397f;
      sa[83] = -0.6453643f;
      sa[84] = -1.5710505f;
      sa[85] = -1.2692964f;
      sa[86] = 0.29056075f;
      sa[87] = 1.2621257f;
      sa[88] = -1.7228329f;
      sa[89] = -0.68319297f;
      sa[90] = -1.1402044f;
      sa[91] = 1.4863682f;
      sa[92] = -0.31424192f;
      sa[93] = 0.33928725f;
      sa[94] = -1.5068306f;
      sa[95] = -0.18302116f;
      sa[96] = -1.069671f;
      sa[97] = 0.20491302f;
      sa[98] = -0.16819175f;
      sa[99] = 0.1254788f;
      sa[100] = -1.2088557f;
      sa[101] = -0.32842085f;
      sa[102] = 1.2710922f;
      sa[103] = 0.054339685f;
      sa[104] = -0.10218474f;
      sa[105] = -0.5367273f;
      sa[106] = 1.5130104f;
      sa[107] = 1.1327028f;
      sa[108] = 0.733215f;
      sa[109] = 0.051452935f;
      sa[110] = 0.6422665f;
      sa[111] = -0.975281f;
      sa[112] = -1.231578f;
      sa[113] = 0.26455474f;
      sa[114] = 0.15695986f;
      sa[115] = -0.2229182f;
      sa[116] = 1.0719104f;
      sa[117] = 0.09230837f;
      sa[118] = -1.4056166f;
      sa[119] = -0.87979823f;
      sa[120] = -0.27485314f;
      sa[121] = 1.1650095f;
      sa[122] = -0.06792735f;
      sa[123] = 0.41645578f;
      sa[124] = -0.757943f;
      sa[125] = 0.8049845f;
      sa[126] = -1.003269f;
      sa[127] = 0.46250373f;
      sa[128] = -0.84378594f;
      sa[129] = 0.79378057f;
      sa[130] = -0.51572657f;
      sa[131] = 0.82016337f;
      sa[132] = 0.4757767f;
      sa[133] = 0.6093358f;
      sa[134] = 0.63147026f;
      sa[135] = -1.0364614f;
      sa[136] = 0.4168686f;
      sa[137] = -0.56646127f;
      sa[138] = -0.064717196f;
      sa[139] = 1.025059f;
      sa[140] = -0.4592376f;
      sa[141] = -0.12048687f;
      sa[142] = 0.32654083f;
      sa[143] = -0.81094635f;
      sa[144] = 0.8043184f;
      sa[145] = -0.8131779f;
      sa[146] = 0.07052684f;
      sa[147] = -0.30030748f;
      sa[148] = -0.93174756f;
      sa[149] = 1.105803f;
      sa[150] = -1.182442f;
      sa[151] = -1.6538707f;
      sa[152] = 1.5762956f;
      sa[153] = -0.7396215f;
      sa[154] = 1.2979007f;
      sa[155] = -1.5312431f;
      sa[156] = -1.398359f;
      sa[157] = -1.1808048f;
      sa[158] = -0.28315705f;
      sa[159] = -0.58601016f;
      sa[160] = 1.2396508f;
      sa[161] = -1.100314f;
      sa[162] = 1.5098577f;
      sa[163] = -0.7593029f;
      sa[164] = 0.69795465f;
      sa[165] = -0.571966f;
      sa[166] = 1.3843002f;
      sa[167] = -1.5059453f;
      sa[168] = 1.0073695f;
      sa[169] = -1.5354975f;
      sa[170] = 1.0888082f;
      sa[171] = 0.9041583f;
      sa[172] = 1.4985849f;
      sa[173] = 1.4558008f;
      sa[174] = -0.16093348f;
      sa[175] = 0.011895697f;
      sa[176] = -0.8753602f;
      sa[177] = 0.047723167f;
      sa[178] = -1.1505884f;
      sa[179] = -0.26739687f;
      sa[180] = -1.4284841f;
      sa[181] = -0.9924549f;
      sa[182] = 0.43773067f;
      sa[183] = -0.92896664f;
      sa[184] = 1.4896134f;
      sa[185] = 1.2835113f;
      sa[186] = -0.48295337f;
      sa[187] = -1.0421085f;
      sa[188] = -0.5222001f;
      sa[189] = -0.81750107f;
      sa[190] = -1.4913574f;
      sa[191] = -0.97208166f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_32x6_ReLU_04 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_32x6_ReLU_04_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_32x6_ReLU_04_0 implements java.io.Serializable {
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
class h2o_nn_32x6_ReLU_04_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_nn_32x6_ReLU_04_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_ReLU_04_ColInfo_13_0 implements java.io.Serializable {
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

