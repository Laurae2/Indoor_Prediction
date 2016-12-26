/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:43:37.186+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_32x6_Tanh_12

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_32x6_Tanh_12 > h2o_nn_32x6_Tanh_12.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_32x6_Tanh_12.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_32x6_Tanh_12", algorithm="deeplearning")
public class h2o_nn_32x6_Tanh_12 extends GenModel {
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
      /* Input */ h2o_nn_32x6_Tanh_12_Activation_0.VALUES,
      /* Tanh */ h2o_nn_32x6_Tanh_12_Activation_1.VALUES,
      /* Softmax */ h2o_nn_32x6_Tanh_12_Activation_2.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_32x6_Tanh_12_Bias_0.VALUES,
      /* Tanh */ h2o_nn_32x6_Tanh_12_Bias_1.VALUES,
      /* Softmax */ h2o_nn_32x6_Tanh_12_Bias_2.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_32x6_Tanh_12_Weight_0.VALUES,
      /* Tanh */ h2o_nn_32x6_Tanh_12_Weight_1.VALUES,
      /* Softmax */ h2o_nn_32x6_Tanh_12_Weight_2.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_32x6_Tanh_12.VALUES;
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
    /* Label */ h2o_nn_32x6_Tanh_12_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.2864864864864865,0.13513513513513514,0.14594594594594595,0.2864864864864865,0.14594594594594595};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_32x6_Tanh_12() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-1182090487665508736L); }

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
class h2o_nn_32x6_Tanh_12_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_32x6_Tanh_12_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_12_Activation_0_0 implements java.io.Serializable {
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
class h2o_nn_32x6_Tanh_12_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_Tanh_12_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_12_Activation_1_0 implements java.io.Serializable {
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
class h2o_nn_32x6_Tanh_12_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_32x6_Tanh_12_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_12_Activation_2_0 implements java.io.Serializable {
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
class h2o_nn_32x6_Tanh_12_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Tanh layer
class h2o_nn_32x6_Tanh_12_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[32];
  static {
    h2o_nn_32x6_Tanh_12_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_12_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.08157260982096533;
      sa[1] = 0.012468443688249117;
      sa[2] = -0.07177511734429043;
      sa[3] = -0.08320618215319407;
      sa[4] = 0.06288594077049219;
      sa[5] = 0.027303986634321455;
      sa[6] = 0.02822472992791128;
      sa[7] = -0.03810472425801233;
      sa[8] = -0.07236738442671736;
      sa[9] = 0.11784383347746452;
      sa[10] = -0.014587082421540196;
      sa[11] = 0.010313537860243813;
      sa[12] = 0.022414776028393202;
      sa[13] = 0.0745759486143417;
      sa[14] = -0.0826480167196022;
      sa[15] = 0.04364104728273978;
      sa[16] = 0.06580764804303808;
      sa[17] = 0.0028777383427325406;
      sa[18] = -0.031870216285057915;
      sa[19] = -0.06149540678814375;
      sa[20] = 0.07504947494398509;
      sa[21] = -0.07062442858331805;
      sa[22] = -0.018720246604339758;
      sa[23] = 0.026032789022944248;
      sa[24] = -0.07937763806664822;
      sa[25] = 0.07558555297432026;
      sa[26] = -0.08371299724783768;
      sa[27] = 0.05738704086452336;
      sa[28] = -0.03452822387856741;
      sa[29] = 0.02420427340707551;
      sa[30] = 0.04272582722491601;
      sa[31] = -0.014683107540890137;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_32x6_Tanh_12_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_32x6_Tanh_12_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_12_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.08033809935520309;
      sa[1] = -0.0723177870181692;
      sa[2] = -0.03260495097041503;
      sa[3] = -0.06490471754781581;
      sa[4] = -0.07217333241650442;
    }
  }
}
class h2o_nn_32x6_Tanh_12_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Tanh layer
class h2o_nn_32x6_Tanh_12_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[416];
  static {
    h2o_nn_32x6_Tanh_12_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_12_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.04561815f;
      sa[1] = 0.27160078f;
      sa[2] = 0.22562109f;
      sa[3] = 0.15526529f;
      sa[4] = -0.26987442f;
      sa[5] = -0.22361828f;
      sa[6] = 0.048995286f;
      sa[7] = 0.2816513f;
      sa[8] = 0.1381543f;
      sa[9] = -0.173881f;
      sa[10] = -0.24991818f;
      sa[11] = -0.21110155f;
      sa[12] = -0.092951976f;
      sa[13] = -0.30592105f;
      sa[14] = -0.24540246f;
      sa[15] = -0.034254733f;
      sa[16] = 0.095895395f;
      sa[17] = 0.11271907f;
      sa[18] = -0.2142461f;
      sa[19] = 0.22345977f;
      sa[20] = 0.28108495f;
      sa[21] = -0.35606283f;
      sa[22] = 0.33556855f;
      sa[23] = -0.14916044f;
      sa[24] = 0.25506482f;
      sa[25] = 0.3091511f;
      sa[26] = -0.34677783f;
      sa[27] = 0.39380807f;
      sa[28] = 0.30199564f;
      sa[29] = -0.23974302f;
      sa[30] = -0.06416261f;
      sa[31] = 0.04582261f;
      sa[32] = 0.27699736f;
      sa[33] = -0.34871483f;
      sa[34] = 0.10670237f;
      sa[35] = -0.2002024f;
      sa[36] = 0.10098558f;
      sa[37] = 0.281072f;
      sa[38] = -0.11367208f;
      sa[39] = 0.21542594f;
      sa[40] = 0.04758035f;
      sa[41] = -0.08798902f;
      sa[42] = -0.09009141f;
      sa[43] = 0.1693009f;
      sa[44] = 0.15777268f;
      sa[45] = -0.24234532f;
      sa[46] = 0.032406818f;
      sa[47] = 0.024225093f;
      sa[48] = 0.11597568f;
      sa[49] = 0.40026435f;
      sa[50] = 0.17144817f;
      sa[51] = -0.25084156f;
      sa[52] = -0.06905346f;
      sa[53] = 0.19116013f;
      sa[54] = 0.28697535f;
      sa[55] = 0.088721044f;
      sa[56] = -0.100246675f;
      sa[57] = -0.43975973f;
      sa[58] = 0.47590408f;
      sa[59] = -0.21569839f;
      sa[60] = -0.34039566f;
      sa[61] = 0.14407036f;
      sa[62] = 0.24209353f;
      sa[63] = -0.03242495f;
      sa[64] = 0.054939847f;
      sa[65] = 0.16474715f;
      sa[66] = -0.034884926f;
      sa[67] = 0.13925111f;
      sa[68] = -0.13866536f;
      sa[69] = 0.1767127f;
      sa[70] = -0.097529694f;
      sa[71] = 0.30762815f;
      sa[72] = -0.15171264f;
      sa[73] = 0.2521625f;
      sa[74] = -0.16629642f;
      sa[75] = 0.13577078f;
      sa[76] = -0.17161998f;
      sa[77] = -0.05956525f;
      sa[78] = -0.06796151f;
      sa[79] = 0.14328688f;
      sa[80] = -0.28597176f;
      sa[81] = -0.22927265f;
      sa[82] = -0.14661862f;
      sa[83] = 0.25485018f;
      sa[84] = -0.12606983f;
      sa[85] = -0.20255706f;
      sa[86] = 0.19094312f;
      sa[87] = 0.38463488f;
      sa[88] = 0.25990388f;
      sa[89] = 0.11764584f;
      sa[90] = -0.07470066f;
      sa[91] = 0.16441955f;
      sa[92] = 0.19427761f;
      sa[93] = -0.22034612f;
      sa[94] = -0.19682796f;
      sa[95] = -0.22646582f;
      sa[96] = 0.15051949f;
      sa[97] = -0.008550592f;
      sa[98] = 0.25772884f;
      sa[99] = -0.10886773f;
      sa[100] = -0.07323006f;
      sa[101] = -0.26771945f;
      sa[102] = -0.25439492f;
      sa[103] = -0.10783315f;
      sa[104] = 0.020146605f;
      sa[105] = -0.17862359f;
      sa[106] = -0.32556117f;
      sa[107] = 0.16559744f;
      sa[108] = 0.21971251f;
      sa[109] = -0.25449225f;
      sa[110] = -0.21860807f;
      sa[111] = 0.17543244f;
      sa[112] = 0.149486f;
      sa[113] = 0.03178664f;
      sa[114] = -0.103527196f;
      sa[115] = -0.10175833f;
      sa[116] = -0.098119035f;
      sa[117] = -0.03663135f;
      sa[118] = -0.33591875f;
      sa[119] = -0.18970573f;
      sa[120] = 0.22129168f;
      sa[121] = 0.19642088f;
      sa[122] = 0.35149112f;
      sa[123] = 0.39004335f;
      sa[124] = 0.3086133f;
      sa[125] = 0.31794345f;
      sa[126] = -0.29308403f;
      sa[127] = -0.28776208f;
      sa[128] = 0.058718074f;
      sa[129] = 0.031058524f;
      sa[130] = -0.19223723f;
      sa[131] = 0.35078406f;
      sa[132] = 0.18628657f;
      sa[133] = -0.3308901f;
      sa[134] = 0.07795843f;
      sa[135] = -0.27559552f;
      sa[136] = -0.26762787f;
      sa[137] = -0.039966755f;
      sa[138] = 0.1224788f;
      sa[139] = -0.13412894f;
      sa[140] = -0.10630409f;
      sa[141] = -0.30726784f;
      sa[142] = 0.39590248f;
      sa[143] = -0.34288913f;
      sa[144] = 0.268319f;
      sa[145] = -0.19933903f;
      sa[146] = -0.3476667f;
      sa[147] = 0.16086568f;
      sa[148] = 0.26490754f;
      sa[149] = 0.2592951f;
      sa[150] = 0.34102282f;
      sa[151] = -0.29687577f;
      sa[152] = 0.10717563f;
      sa[153] = 0.3635436f;
      sa[154] = 0.2192604f;
      sa[155] = -0.10106672f;
      sa[156] = 0.112482786f;
      sa[157] = -0.19494197f;
      sa[158] = 0.12348567f;
      sa[159] = 0.07566422f;
      sa[160] = 0.034165453f;
      sa[161] = 0.26391208f;
      sa[162] = -0.16879475f;
      sa[163] = -0.098026514f;
      sa[164] = 0.27455598f;
      sa[165] = -0.03326226f;
      sa[166] = -0.12643656f;
      sa[167] = -0.1601277f;
      sa[168] = 0.21886326f;
      sa[169] = 0.3390657f;
      sa[170] = -0.029272787f;
      sa[171] = -0.04208778f;
      sa[172] = -0.11479123f;
      sa[173] = -0.23423594f;
      sa[174] = -0.0074550724f;
      sa[175] = -0.22848727f;
      sa[176] = -0.060597904f;
      sa[177] = -0.06084276f;
      sa[178] = -0.01276026f;
      sa[179] = 0.13981548f;
      sa[180] = -0.14107294f;
      sa[181] = -0.2813046f;
      sa[182] = -0.14060718f;
      sa[183] = 0.09624308f;
      sa[184] = 0.32024372f;
      sa[185] = 0.22461426f;
      sa[186] = -0.28270194f;
      sa[187] = 0.09891107f;
      sa[188] = -0.11729665f;
      sa[189] = 0.2743642f;
      sa[190] = 0.07309127f;
      sa[191] = -0.08107758f;
      sa[192] = -0.1020587f;
      sa[193] = 0.25724423f;
      sa[194] = 0.2960165f;
      sa[195] = 0.31858853f;
      sa[196] = 0.35487175f;
      sa[197] = 0.0071077254f;
      sa[198] = 0.10927305f;
      sa[199] = -0.048991848f;
      sa[200] = -0.00704828f;
      sa[201] = -0.35955384f;
      sa[202] = 0.15340778f;
      sa[203] = -0.03308365f;
      sa[204] = -0.21382412f;
      sa[205] = 0.1983831f;
      sa[206] = 0.3546687f;
      sa[207] = -0.019223386f;
      sa[208] = -0.31448826f;
      sa[209] = -0.15166177f;
      sa[210] = -0.34616333f;
      sa[211] = -0.35785982f;
      sa[212] = 0.20305757f;
      sa[213] = -0.27671227f;
      sa[214] = -0.05050597f;
      sa[215] = 0.08364559f;
      sa[216] = 0.10050969f;
      sa[217] = 0.29052514f;
      sa[218] = -0.17584307f;
      sa[219] = 0.33949274f;
      sa[220] = -0.34661457f;
      sa[221] = -0.012987952f;
      sa[222] = 0.25880152f;
      sa[223] = 0.07066459f;
      sa[224] = 0.10602671f;
      sa[225] = 0.315597f;
      sa[226] = -0.2556964f;
      sa[227] = 0.41945428f;
      sa[228] = 0.12106303f;
      sa[229] = 0.08198422f;
      sa[230] = 0.27538052f;
      sa[231] = -0.32681143f;
      sa[232] = -0.4104436f;
      sa[233] = 0.2073217f;
      sa[234] = 0.16279086f;
      sa[235] = -0.28251347f;
      sa[236] = -0.015550024f;
      sa[237] = -0.18650658f;
      sa[238] = -0.13822117f;
      sa[239] = 0.044165026f;
      sa[240] = 0.086659424f;
      sa[241] = -0.26037696f;
      sa[242] = -0.098627366f;
      sa[243] = -0.3476056f;
      sa[244] = -0.2733977f;
      sa[245] = -0.20185566f;
      sa[246] = -0.36281964f;
      sa[247] = -0.04545018f;
      sa[248] = 0.31556106f;
      sa[249] = 0.28503498f;
      sa[250] = -0.23766421f;
      sa[251] = -0.09016355f;
      sa[252] = -0.29980257f;
      sa[253] = -0.49822634f;
      sa[254] = -0.284034f;
      sa[255] = 0.23990755f;
      sa[256] = -0.20692483f;
      sa[257] = 0.3665769f;
      sa[258] = -0.30525014f;
      sa[259] = 0.11119296f;
      sa[260] = -0.2603001f;
      sa[261] = -0.103409626f;
      sa[262] = -0.15805037f;
      sa[263] = 0.20229849f;
      sa[264] = -0.31973466f;
      sa[265] = 0.24468337f;
      sa[266] = -0.5779784f;
      sa[267] = -0.27356523f;
      sa[268] = 0.15180515f;
      sa[269] = -0.07526672f;
      sa[270] = -0.16710041f;
      sa[271] = 0.29956147f;
      sa[272] = -0.22206363f;
      sa[273] = -0.11606983f;
      sa[274] = 0.12856054f;
      sa[275] = -0.035578623f;
      sa[276] = 0.11503369f;
      sa[277] = -0.19488277f;
      sa[278] = -0.10973534f;
      sa[279] = 0.120933846f;
      sa[280] = -0.20019627f;
      sa[281] = 0.33988032f;
      sa[282] = -0.104987405f;
      sa[283] = -6.0410134E-4f;
      sa[284] = 0.12222f;
      sa[285] = -0.08078935f;
      sa[286] = 0.28462437f;
      sa[287] = 0.18749315f;
      sa[288] = -0.18414858f;
      sa[289] = -0.109979585f;
      sa[290] = -0.040356334f;
      sa[291] = -0.06324751f;
      sa[292] = 0.19678833f;
      sa[293] = -0.106916286f;
      sa[294] = 0.09776639f;
      sa[295] = 0.06350075f;
      sa[296] = 0.25423792f;
      sa[297] = 0.22676286f;
      sa[298] = -0.14139286f;
      sa[299] = -0.047204036f;
      sa[300] = 0.03843753f;
      sa[301] = -0.026540836f;
      sa[302] = 0.188509f;
      sa[303] = 0.050323226f;
      sa[304] = -0.27624083f;
      sa[305] = -0.061441936f;
      sa[306] = 0.2405878f;
      sa[307] = -0.19152802f;
      sa[308] = 0.0826269f;
      sa[309] = 0.105242684f;
      sa[310] = 0.40560976f;
      sa[311] = 0.15634507f;
      sa[312] = -0.24681959f;
      sa[313] = -0.2215223f;
      sa[314] = -0.2350811f;
      sa[315] = 0.016516786f;
      sa[316] = 0.007484869f;
      sa[317] = -0.12919238f;
      sa[318] = 0.1233485f;
      sa[319] = 0.3084823f;
      sa[320] = -0.07129129f;
      sa[321] = 0.14548497f;
      sa[322] = 0.3067298f;
      sa[323] = -0.2597873f;
      sa[324] = 0.17250203f;
      sa[325] = 0.34055674f;
      sa[326] = -0.34183362f;
      sa[327] = 0.010288751f;
      sa[328] = -0.17272155f;
      sa[329] = 0.20020181f;
      sa[330] = -0.113314115f;
      sa[331] = -0.021065522f;
      sa[332] = -0.30448702f;
      sa[333] = 0.15742312f;
      sa[334] = 0.0105058225f;
      sa[335] = 0.1881418f;
      sa[336] = -0.20822093f;
      sa[337] = -0.1242204f;
      sa[338] = 0.33806413f;
      sa[339] = -0.26229683f;
      sa[340] = -0.4050341f;
      sa[341] = -0.2608072f;
      sa[342] = 0.27726346f;
      sa[343] = -0.10724636f;
      sa[344] = -0.22345486f;
      sa[345] = 0.23640586f;
      sa[346] = 0.24122763f;
      sa[347] = -0.20368369f;
      sa[348] = 0.4055857f;
      sa[349] = -0.3128468f;
      sa[350] = -0.15395118f;
      sa[351] = 0.12449417f;
      sa[352] = 0.25280705f;
      sa[353] = -0.018399261f;
      sa[354] = 0.05695094f;
      sa[355] = 0.043876372f;
      sa[356] = -0.1455866f;
      sa[357] = 0.27914953f;
      sa[358] = -0.33333623f;
      sa[359] = 0.040871866f;
      sa[360] = 0.14232571f;
      sa[361] = -0.1016914f;
      sa[362] = -0.17926927f;
      sa[363] = 0.11255036f;
      sa[364] = -0.122942924f;
      sa[365] = -0.18323563f;
      sa[366] = 0.0320668f;
      sa[367] = -0.2748962f;
      sa[368] = -0.137f;
      sa[369] = 0.2751224f;
      sa[370] = -0.2799667f;
      sa[371] = 0.14210762f;
      sa[372] = -4.0920448E-4f;
      sa[373] = 0.27797773f;
      sa[374] = -0.19179629f;
      sa[375] = -0.2824156f;
      sa[376] = -0.38669047f;
      sa[377] = -0.2628602f;
      sa[378] = 0.37495247f;
      sa[379] = 0.090642534f;
      sa[380] = -0.17673646f;
      sa[381] = -0.14271984f;
      sa[382] = 0.0019555364f;
      sa[383] = -0.11486409f;
      sa[384] = -0.1850252f;
      sa[385] = 0.116585076f;
      sa[386] = -0.26351693f;
      sa[387] = -0.27737227f;
      sa[388] = 0.014863084f;
      sa[389] = 0.08427916f;
      sa[390] = 0.020243809f;
      sa[391] = -0.12786055f;
      sa[392] = 0.29247668f;
      sa[393] = 0.18854503f;
      sa[394] = -0.12643436f;
      sa[395] = 0.42410204f;
      sa[396] = 0.25441816f;
      sa[397] = -0.30831006f;
      sa[398] = 0.14671108f;
      sa[399] = 0.21606126f;
      sa[400] = 0.20931375f;
      sa[401] = 0.41945878f;
      sa[402] = -0.3094693f;
      sa[403] = 0.10825472f;
      sa[404] = 0.35240853f;
      sa[405] = -0.25387326f;
      sa[406] = -0.06724444f;
      sa[407] = 0.22782472f;
      sa[408] = -0.23312452f;
      sa[409] = -0.36245987f;
      sa[410] = -0.11460185f;
      sa[411] = 0.22376542f;
      sa[412] = -0.17544474f;
      sa[413] = 0.085330024f;
      sa[414] = 0.15705329f;
      sa[415] = 0.3610807f;
    }
  }
}
// Neuron weights connecting Tanh and Softmax layer
class h2o_nn_32x6_Tanh_12_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[160];
  static {
    h2o_nn_32x6_Tanh_12_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_12_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 1.1902889f;
      sa[1] = -0.1563914f;
      sa[2] = -0.43685597f;
      sa[3] = -0.7895069f;
      sa[4] = 0.05230677f;
      sa[5] = 0.15735996f;
      sa[6] = 1.3467829f;
      sa[7] = -0.6146872f;
      sa[8] = -0.94597954f;
      sa[9] = 0.58443683f;
      sa[10] = -0.2661062f;
      sa[11] = 0.52409345f;
      sa[12] = 0.1471521f;
      sa[13] = 0.17807092f;
      sa[14] = -1.2247527f;
      sa[15] = 1.120561f;
      sa[16] = 1.7415831f;
      sa[17] = -0.59418344f;
      sa[18] = 0.10190395f;
      sa[19] = -0.55285424f;
      sa[20] = 1.2561997f;
      sa[21] = 0.098627806f;
      sa[22] = -0.93126065f;
      sa[23] = 0.74244887f;
      sa[24] = -1.4275775f;
      sa[25] = 0.42013845f;
      sa[26] = -0.24215534f;
      sa[27] = 1.4247435f;
      sa[28] = -1.3511692f;
      sa[29] = -0.4186901f;
      sa[30] = 0.5189479f;
      sa[31] = -0.73480946f;
      sa[32] = -1.1492819f;
      sa[33] = -1.481933f;
      sa[34] = 0.05615932f;
      sa[35] = 1.4114126f;
      sa[36] = -0.34654096f;
      sa[37] = -0.08262875f;
      sa[38] = 0.1440142f;
      sa[39] = 0.7856592f;
      sa[40] = -0.6678288f;
      sa[41] = -1.3563313f;
      sa[42] = -1.0417734f;
      sa[43] = -1.3726315f;
      sa[44] = -0.014465527f;
      sa[45] = -1.3291022f;
      sa[46] = 1.2073748f;
      sa[47] = 1.495105f;
      sa[48] = -1.2447895f;
      sa[49] = -1.0127565f;
      sa[50] = 0.37615722f;
      sa[51] = -0.32006973f;
      sa[52] = 0.8574897f;
      sa[53] = 0.8943282f;
      sa[54] = -0.23284313f;
      sa[55] = 1.162343f;
      sa[56] = -1.1348804f;
      sa[57] = -1.1141752f;
      sa[58] = 0.17964238f;
      sa[59] = -0.19743581f;
      sa[60] = 1.3419799f;
      sa[61] = -0.54698724f;
      sa[62] = -0.33441982f;
      sa[63] = 0.72396994f;
      sa[64] = 1.5356998f;
      sa[65] = -0.6195363f;
      sa[66] = 0.58870465f;
      sa[67] = 1.5294293f;
      sa[68] = 1.0084245f;
      sa[69] = -0.941405f;
      sa[70] = -1.6472657f;
      sa[71] = -0.44304198f;
      sa[72] = -1.2068961f;
      sa[73] = 1.2121781f;
      sa[74] = 1.3382391f;
      sa[75] = -0.14552885f;
      sa[76] = 0.8614714f;
      sa[77] = 0.4494939f;
      sa[78] = -1.3334045f;
      sa[79] = 0.92125666f;
      sa[80] = -0.8334194f;
      sa[81] = 0.36416504f;
      sa[82] = 0.8380829f;
      sa[83] = -0.585853f;
      sa[84] = -1.6634976f;
      sa[85] = -1.2341279f;
      sa[86] = 0.21214272f;
      sa[87] = 1.2176324f;
      sa[88] = -1.4874277f;
      sa[89] = -0.46989205f;
      sa[90] = -1.033182f;
      sa[91] = 1.6483876f;
      sa[92] = -0.39888602f;
      sa[93] = 0.25230217f;
      sa[94] = -1.5800238f;
      sa[95] = -0.24476123f;
      sa[96] = -1.0848256f;
      sa[97] = 0.1777742f;
      sa[98] = -0.11003329f;
      sa[99] = 0.23287055f;
      sa[100] = -1.258794f;
      sa[101] = -0.5148969f;
      sa[102] = 1.3405097f;
      sa[103] = 0.08099363f;
      sa[104] = -0.26965967f;
      sa[105] = -0.59724224f;
      sa[106] = 1.5880363f;
      sa[107] = 1.1140606f;
      sa[108] = 0.83335763f;
      sa[109] = 0.070201255f;
      sa[110] = 0.8478411f;
      sa[111] = -0.7931314f;
      sa[112] = -1.3025024f;
      sa[113] = 0.1722282f;
      sa[114] = 0.17497115f;
      sa[115] = -0.24307115f;
      sa[116] = 1.1456286f;
      sa[117] = 0.0259008f;
      sa[118] = -1.4055649f;
      sa[119] = -0.7963931f;
      sa[120] = -0.266387f;
      sa[121] = 0.9017451f;
      sa[122] = -0.117032565f;
      sa[123] = 0.38122496f;
      sa[124] = -0.6995658f;
      sa[125] = 0.8369347f;
      sa[126] = -1.0766114f;
      sa[127] = 0.5199275f;
      sa[128] = -0.9874441f;
      sa[129] = 0.8267488f;
      sa[130] = -0.442765f;
      sa[131] = 0.81586707f;
      sa[132] = 0.55024177f;
      sa[133] = 0.68272376f;
      sa[134] = 0.59721905f;
      sa[135] = -1.2247111f;
      sa[136] = 0.52288187f;
      sa[137] = -0.55913216f;
      sa[138] = -0.09502825f;
      sa[139] = 1.132356f;
      sa[140] = -0.43675166f;
      sa[141] = -0.12910669f;
      sa[142] = 0.27812588f;
      sa[143] = -0.76809275f;
      sa[144] = 0.89248407f;
      sa[145] = -0.7366299f;
      sa[146] = -0.027500803f;
      sa[147] = -0.28812107f;
      sa[148] = -0.91457206f;
      sa[149] = 1.0942727f;
      sa[150] = -1.0765276f;
      sa[151] = -1.4405102f;
      sa[152] = 1.6517785f;
      sa[153] = -0.55321294f;
      sa[154] = 1.3502833f;
      sa[155] = -1.468676f;
      sa[156] = -1.51309f;
      sa[157] = -1.1697898f;
      sa[158] = -0.23772046f;
      sa[159] = -0.5204175f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_32x6_Tanh_12 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_32x6_Tanh_12_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_32x6_Tanh_12_0 implements java.io.Serializable {
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
class h2o_nn_32x6_Tanh_12_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    h2o_nn_32x6_Tanh_12_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_32x6_Tanh_12_ColInfo_13_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "4";
      sa[3] = "5";
      sa[4] = "6";
    }
  }
}

