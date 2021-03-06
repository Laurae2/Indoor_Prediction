/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:43:25.873+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_16x16x6_Tanh_11

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_16x16x6_Tanh_11 > h2o_nn_16x16x6_Tanh_11.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_16x16x6_Tanh_11.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_16x16x6_Tanh_11", algorithm="deeplearning")
public class h2o_nn_16x16x6_Tanh_11 extends GenModel {
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
      /* Input */ h2o_nn_16x16x6_Tanh_11_Activation_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_11_Activation_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_11_Activation_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_11_Activation_3.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_16x16x6_Tanh_11_Bias_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_11_Bias_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_11_Bias_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_11_Bias_3.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_16x16x6_Tanh_11_Weight_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_11_Weight_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_11_Weight_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_11_Weight_3.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_16x16x6_Tanh_11.VALUES;
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
    /* Label */ h2o_nn_16x16x6_Tanh_11_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.2548076923076923,0.18269230769230768,0.057692307692307696,0.125,0.2548076923076923,0.125};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_16x16x6_Tanh_11() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(-5406727010015812496L); }

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
class h2o_nn_16x16x6_Tanh_11_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_16x16x6_Tanh_11_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_Activation_0_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_11_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_11_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_Activation_1_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_11_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_11_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_Activation_2_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_11_Activation_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_16x16x6_Tanh_11_Activation_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_Activation_3_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_11_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Tanh layer
class h2o_nn_16x16x6_Tanh_11_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_11_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = -0.12768412970215087;
      sa[1] = 0.16702855660511912;
      sa[2] = 0.05186668081480807;
      sa[3] = 0.03223936526802694;
      sa[4] = 0.11494232416938457;
      sa[5] = 0.15310639765400452;
      sa[6] = -0.0029085465760048303;
      sa[7] = -0.09134929040625706;
      sa[8] = 0.06077942605745189;
      sa[9] = -0.10523901331868173;
      sa[10] = -0.025196217077874034;
      sa[11] = 0.006984484761285407;
      sa[12] = 0.015006648759792292;
      sa[13] = 0.04977456550426019;
      sa[14] = -0.08030528460253074;
      sa[15] = -0.05168080406522856;
    }
  }
}
// Neuron bias values for Tanh layer
class h2o_nn_16x16x6_Tanh_11_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_11_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.036153937562798576;
      sa[1] = -0.298122899772991;
      sa[2] = -0.3055505722806027;
      sa[3] = 0.1476700244179046;
      sa[4] = -0.05543639274637335;
      sa[5] = 0.16122912737315645;
      sa[6] = 0.008658870433598594;
      sa[7] = 0.34056137069726317;
      sa[8] = -0.11417356257891835;
      sa[9] = 0.060496999685335166;
      sa[10] = 0.16983686358741573;
      sa[11] = 0.45112837781407533;
      sa[12] = -0.07015230937510303;
      sa[13] = -0.0135815128583097;
      sa[14] = -0.18547883777494845;
      sa[15] = -0.18083889100147438;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_16x16x6_Tanh_11_Bias_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[6];
  static {
    h2o_nn_16x16x6_Tanh_11_Bias_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_Bias_3_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.2032929647855042;
      sa[1] = -0.11433122351042027;
      sa[2] = -0.2971710663177113;
      sa[3] = 0.07919114995944548;
      sa[4] = 0.16726890769025374;
      sa[5] = -0.24456576947346206;
    }
  }
}
class h2o_nn_16x16x6_Tanh_11_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Tanh layer
class h2o_nn_16x16x6_Tanh_11_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[208];
  static {
    h2o_nn_16x16x6_Tanh_11_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.037466444f;
      sa[1] = 0.19356601f;
      sa[2] = 0.16491313f;
      sa[3] = 0.30424818f;
      sa[4] = -0.105851725f;
      sa[5] = -0.26165092f;
      sa[6] = -0.3003843f;
      sa[7] = 0.3704679f;
      sa[8] = 0.10837991f;
      sa[9] = -0.2405026f;
      sa[10] = -0.11547952f;
      sa[11] = -0.34167546f;
      sa[12] = 0.040141955f;
      sa[13] = -0.2592073f;
      sa[14] = -0.27017227f;
      sa[15] = 0.1710485f;
      sa[16] = 0.028864762f;
      sa[17] = 0.14147232f;
      sa[18] = -0.18712255f;
      sa[19] = 0.30175862f;
      sa[20] = 0.4627151f;
      sa[21] = -0.39008558f;
      sa[22] = 0.5210122f;
      sa[23] = 0.016341709f;
      sa[24] = 0.3063611f;
      sa[25] = 0.48378232f;
      sa[26] = -0.46691388f;
      sa[27] = 0.6896085f;
      sa[28] = 0.23056215f;
      sa[29] = -0.35520348f;
      sa[30] = -0.44607398f;
      sa[31] = 0.14067483f;
      sa[32] = 0.5551772f;
      sa[33] = -0.517697f;
      sa[34] = 0.1358872f;
      sa[35] = -0.11998853f;
      sa[36] = -0.1686504f;
      sa[37] = 0.4349337f;
      sa[38] = -0.15461552f;
      sa[39] = 0.21582806f;
      sa[40] = -0.06961681f;
      sa[41] = -0.24095352f;
      sa[42] = -0.30125645f;
      sa[43] = -0.21549194f;
      sa[44] = 0.378983f;
      sa[45] = -0.6048691f;
      sa[46] = 0.0025519228f;
      sa[47] = -0.031733353f;
      sa[48] = 0.31629324f;
      sa[49] = 0.41692385f;
      sa[50] = 0.3464105f;
      sa[51] = -0.5172001f;
      sa[52] = -0.14408164f;
      sa[53] = 0.27604455f;
      sa[54] = 0.21512285f;
      sa[55] = -0.0779713f;
      sa[56] = -0.5058915f;
      sa[57] = -0.4251107f;
      sa[58] = 0.6100233f;
      sa[59] = -0.31149516f;
      sa[60] = -0.4029327f;
      sa[61] = 0.28239152f;
      sa[62] = -0.06759114f;
      sa[63] = 0.14274865f;
      sa[64] = 0.030035082f;
      sa[65] = 0.08761684f;
      sa[66] = 0.109496474f;
      sa[67] = 0.26988685f;
      sa[68] = -0.056646198f;
      sa[69] = 0.15919903f;
      sa[70] = -0.34463146f;
      sa[71] = 0.53675246f;
      sa[72] = -0.1324672f;
      sa[73] = 0.4388475f;
      sa[74] = -0.38488707f;
      sa[75] = -0.0063636936f;
      sa[76] = -0.29046902f;
      sa[77] = -0.2290681f;
      sa[78] = -0.054406013f;
      sa[79] = 0.3937697f;
      sa[80] = -0.20460539f;
      sa[81] = -0.16457744f;
      sa[82] = -0.12562408f;
      sa[83] = 0.3386603f;
      sa[84] = -0.07009676f;
      sa[85] = -0.33115548f;
      sa[86] = 0.2483142f;
      sa[87] = 0.3459947f;
      sa[88] = 0.20139706f;
      sa[89] = 0.13375595f;
      sa[90] = -0.17008215f;
      sa[91] = -0.21171051f;
      sa[92] = -0.024942473f;
      sa[93] = -0.22685538f;
      sa[94] = -0.5464312f;
      sa[95] = -0.78698957f;
      sa[96] = 0.042301748f;
      sa[97] = 0.32660544f;
      sa[98] = 0.5456852f;
      sa[99] = 0.37252888f;
      sa[100] = -0.21855439f;
      sa[101] = -0.43123633f;
      sa[102] = -0.24425471f;
      sa[103] = -0.118342966f;
      sa[104] = 0.13062178f;
      sa[105] = 0.044807296f;
      sa[106] = -0.22065763f;
      sa[107] = 0.26098993f;
      sa[108] = 0.42684814f;
      sa[109] = -0.18089652f;
      sa[110] = -0.48560932f;
      sa[111] = 0.09457484f;
      sa[112] = 0.08305711f;
      sa[113] = 0.13959752f;
      sa[114] = -0.38279948f;
      sa[115] = 0.105502315f;
      sa[116] = -0.37891072f;
      sa[117] = -0.07269105f;
      sa[118] = -0.5940022f;
      sa[119] = -0.24615908f;
      sa[120] = 0.1777367f;
      sa[121] = 0.26125628f;
      sa[122] = 0.483045f;
      sa[123] = 0.35791343f;
      sa[124] = 0.46378127f;
      sa[125] = 0.38821352f;
      sa[126] = -0.371956f;
      sa[127] = -0.12034018f;
      sa[128] = 0.11501172f;
      sa[129] = 0.3175275f;
      sa[130] = -0.3853936f;
      sa[131] = 0.43991295f;
      sa[132] = 0.24769361f;
      sa[133] = -0.5622549f;
      sa[134] = -0.11065644f;
      sa[135] = -0.36564082f;
      sa[136] = -0.21859038f;
      sa[137] = 0.030488316f;
      sa[138] = 0.29666954f;
      sa[139] = -0.20501743f;
      sa[140] = -0.33334884f;
      sa[141] = -0.1265073f;
      sa[142] = 0.15703084f;
      sa[143] = -0.5133973f;
      sa[144] = 0.5971969f;
      sa[145] = -0.39894074f;
      sa[146] = -0.2589399f;
      sa[147] = 0.13811201f;
      sa[148] = 0.034978457f;
      sa[149] = 0.46449935f;
      sa[150] = 0.4059109f;
      sa[151] = -0.40987727f;
      sa[152] = 0.07702899f;
      sa[153] = 0.28781655f;
      sa[154] = 0.3071548f;
      sa[155] = -0.29273644f;
      sa[156] = 0.09572005f;
      sa[157] = -0.30962923f;
      sa[158] = 0.14715882f;
      sa[159] = -0.10012353f;
      sa[160] = -0.09708619f;
      sa[161] = 0.22747141f;
      sa[162] = -0.034086425f;
      sa[163] = 0.05712755f;
      sa[164] = 0.5269303f;
      sa[165] = -0.12194378f;
      sa[166] = -0.21975447f;
      sa[167] = -0.06385501f;
      sa[168] = 0.28699595f;
      sa[169] = 0.5204555f;
      sa[170] = -0.072046526f;
      sa[171] = -0.1524702f;
      sa[172] = -0.11428987f;
      sa[173] = -0.26635787f;
      sa[174] = 0.034878973f;
      sa[175] = -0.22341886f;
      sa[176] = -0.12828848f;
      sa[177] = -0.17999245f;
      sa[178] = 0.13319433f;
      sa[179] = 0.28724974f;
      sa[180] = 0.014572861f;
      sa[181] = -0.44712687f;
      sa[182] = -0.22444125f;
      sa[183] = 0.29408607f;
      sa[184] = 0.5640803f;
      sa[185] = 0.2308565f;
      sa[186] = -0.38205966f;
      sa[187] = 0.2332678f;
      sa[188] = -0.49499795f;
      sa[189] = 0.2896683f;
      sa[190] = 0.16890705f;
      sa[191] = -0.124473f;
      sa[192] = -0.7163985f;
      sa[193] = 0.46006575f;
      sa[194] = -0.036167137f;
      sa[195] = 0.3159018f;
      sa[196] = 0.26262757f;
      sa[197] = -0.14272374f;
      sa[198] = 0.14029883f;
      sa[199] = -0.10542723f;
      sa[200] = 0.016327063f;
      sa[201] = -0.481445f;
      sa[202] = 0.15942734f;
      sa[203] = -0.113033384f;
      sa[204] = -0.287923f;
      sa[205] = 0.45740676f;
      sa[206] = 0.36286694f;
      sa[207] = 0.11597125f;
    }
  }
}
// Neuron weights connecting Tanh and Tanh layer
class h2o_nn_16x16x6_Tanh_11_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[256];
  static {
    h2o_nn_16x16x6_Tanh_11_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.52575827f;
      sa[1] = 0.10556473f;
      sa[2] = -0.40107906f;
      sa[3] = -0.46143612f;
      sa[4] = 2.8351028E-4f;
      sa[5] = 0.23966517f;
      sa[6] = 0.07337096f;
      sa[7] = -0.1515711f;
      sa[8] = -0.53399414f;
      sa[9] = 0.5436326f;
      sa[10] = -0.08000127f;
      sa[11] = 0.054996554f;
      sa[12] = 0.22166361f;
      sa[13] = -0.21045476f;
      sa[14] = -0.88452184f;
      sa[15] = 0.2231463f;
      sa[16] = 0.6850828f;
      sa[17] = -0.2223695f;
      sa[18] = -0.03403776f;
      sa[19] = -0.28915322f;
      sa[20] = 0.3657561f;
      sa[21] = 0.1650168f;
      sa[22] = -0.5146796f;
      sa[23] = 0.505455f;
      sa[24] = 0.10203405f;
      sa[25] = 0.15377375f;
      sa[26] = 0.100793615f;
      sa[27] = 0.27162102f;
      sa[28] = -0.43880326f;
      sa[29] = -0.20995885f;
      sa[30] = 0.27957508f;
      sa[31] = -0.2886791f;
      sa[32] = -0.13368936f;
      sa[33] = -0.4062054f;
      sa[34] = -0.13025713f;
      sa[35] = 0.23276462f;
      sa[36] = 0.1246204f;
      sa[37] = 0.14760067f;
      sa[38] = -0.06760131f;
      sa[39] = 0.14277524f;
      sa[40] = 0.07335415f;
      sa[41] = -0.46360466f;
      sa[42] = -0.42378557f;
      sa[43] = -0.3573998f;
      sa[44] = -0.2733288f;
      sa[45] = -0.35334656f;
      sa[46] = -0.03638564f;
      sa[47] = 0.39266884f;
      sa[48] = -0.2497689f;
      sa[49] = -0.22612217f;
      sa[50] = 0.23621808f;
      sa[51] = -0.2080384f;
      sa[52] = 0.34299412f;
      sa[53] = 0.34821245f;
      sa[54] = -0.09367947f;
      sa[55] = 0.43074074f;
      sa[56] = -0.30830777f;
      sa[57] = -0.30565876f;
      sa[58] = 0.23572782f;
      sa[59] = -0.022919215f;
      sa[60] = 0.32261062f;
      sa[61] = -0.34223336f;
      sa[62] = 0.06903289f;
      sa[63] = 0.10531071f;
      sa[64] = 0.16121157f;
      sa[65] = -0.046505906f;
      sa[66] = 0.17439176f;
      sa[67] = 0.43937185f;
      sa[68] = 0.29338792f;
      sa[69] = -0.18712327f;
      sa[70] = -0.41316032f;
      sa[71] = -0.07933734f;
      sa[72] = -0.44336668f;
      sa[73] = 0.25327724f;
      sa[74] = 0.2519577f;
      sa[75] = 0.07765439f;
      sa[76] = 0.08478664f;
      sa[77] = 0.13346381f;
      sa[78] = -0.616361f;
      sa[79] = 0.10533611f;
      sa[80] = -0.09464435f;
      sa[81] = 0.1865298f;
      sa[82] = 0.009827714f;
      sa[83] = -0.28747645f;
      sa[84] = -0.30556217f;
      sa[85] = -0.25222528f;
      sa[86] = 0.024041407f;
      sa[87] = 0.10576077f;
      sa[88] = -0.52811223f;
      sa[89] = -0.0573675f;
      sa[90] = -0.429371f;
      sa[91] = 0.38333082f;
      sa[92] = -0.07236532f;
      sa[93] = -0.021621121f;
      sa[94] = -0.8069618f;
      sa[95] = -0.004689829f;
      sa[96] = -0.07171514f;
      sa[97] = 0.045983773f;
      sa[98] = -0.06773359f;
      sa[99] = -0.0867943f;
      sa[100] = -0.24221411f;
      sa[101] = 0.059222866f;
      sa[102] = 0.18057087f;
      sa[103] = 0.14425148f;
      sa[104] = -0.0013445952f;
      sa[105] = -0.09183932f;
      sa[106] = 0.48623672f;
      sa[107] = 0.25289804f;
      sa[108] = 0.19539516f;
      sa[109] = -0.09869074f;
      sa[110] = 0.13397625f;
      sa[111] = -0.25671074f;
      sa[112] = -0.28121313f;
      sa[113] = 0.27164352f;
      sa[114] = 0.06455878f;
      sa[115] = -0.2065339f;
      sa[116] = 0.58305836f;
      sa[117] = 0.41219205f;
      sa[118] = -0.3667415f;
      sa[119] = -0.21009293f;
      sa[120] = -0.28517863f;
      sa[121] = 0.23870954f;
      sa[122] = -0.22798514f;
      sa[123] = 0.15286525f;
      sa[124] = -0.34238377f;
      sa[125] = 0.09492471f;
      sa[126] = -0.85236955f;
      sa[127] = -0.14584886f;
      sa[128] = -0.31463993f;
      sa[129] = 0.181051f;
      sa[130] = 0.36132953f;
      sa[131] = 0.263867f;
      sa[132] = 0.25847107f;
      sa[133] = 0.06635298f;
      sa[134] = 0.43496585f;
      sa[135] = -0.27628025f;
      sa[136] = 0.2069677f;
      sa[137] = -0.37352815f;
      sa[138] = -0.027981516f;
      sa[139] = 0.56542367f;
      sa[140] = -0.49571162f;
      sa[141] = -0.0043387692f;
      sa[142] = 0.3666521f;
      sa[143] = -0.20519759f;
      sa[144] = 0.32783207f;
      sa[145] = -0.2898176f;
      sa[146] = 0.07300097f;
      sa[147] = 0.017812995f;
      sa[148] = -0.42684647f;
      sa[149] = 0.003476128f;
      sa[150] = -0.20581159f;
      sa[151] = -0.66641027f;
      sa[152] = 0.5686908f;
      sa[153] = -0.12880759f;
      sa[154] = 0.3776039f;
      sa[155] = -0.5114094f;
      sa[156] = -0.17979528f;
      sa[157] = -0.21314582f;
      sa[158] = 0.30192462f;
      sa[159] = -0.02800927f;
      sa[160] = 0.40280598f;
      sa[161] = -0.1519263f;
      sa[162] = 0.52703726f;
      sa[163] = -0.31402636f;
      sa[164] = 0.49324164f;
      sa[165] = 0.037470516f;
      sa[166] = 0.38179293f;
      sa[167] = -0.40706423f;
      sa[168] = 0.3771337f;
      sa[169] = -0.61976516f;
      sa[170] = 0.18120165f;
      sa[171] = 0.33152542f;
      sa[172] = 0.21106744f;
      sa[173] = 0.33494982f;
      sa[174] = -0.20515537f;
      sa[175] = -0.14644033f;
      sa[176] = -0.24630676f;
      sa[177] = 0.072822176f;
      sa[178] = -0.21828556f;
      sa[179] = -0.044038378f;
      sa[180] = -0.46974605f;
      sa[181] = -0.4042361f;
      sa[182] = 0.17794481f;
      sa[183] = -0.30820996f;
      sa[184] = 0.075735204f;
      sa[185] = 0.41377255f;
      sa[186] = -0.047793012f;
      sa[187] = -0.35034105f;
      sa[188] = 0.3270266f;
      sa[189] = -0.24575303f;
      sa[190] = -0.28497374f;
      sa[191] = -0.23207481f;
      sa[192] = -0.2668532f;
      sa[193] = 0.12245128f;
      sa[194] = -0.20384817f;
      sa[195] = -0.12015919f;
      sa[196] = 0.12184806f;
      sa[197] = -0.3027825f;
      sa[198] = 0.10703478f;
      sa[199] = 0.35444614f;
      sa[200] = -0.059302446f;
      sa[201] = 0.21316332f;
      sa[202] = 0.06553602f;
      sa[203] = -0.324685f;
      sa[204] = 0.088213f;
      sa[205] = 0.44493574f;
      sa[206] = -0.09391312f;
      sa[207] = 0.14536154f;
      sa[208] = -0.17381288f;
      sa[209] = 0.30216625f;
      sa[210] = -0.2784127f;
      sa[211] = -0.44639987f;
      sa[212] = -0.16644569f;
      sa[213] = 0.2689963f;
      sa[214] = -0.5038852f;
      sa[215] = -0.13168605f;
      sa[216] = -0.13589653f;
      sa[217] = 0.5776792f;
      sa[218] = -0.12038968f;
      sa[219] = 0.1395809f;
      sa[220] = 0.18591096f;
      sa[221] = -0.21045332f;
      sa[222] = -0.7960501f;
      sa[223] = -0.3621803f;
      sa[224] = 0.3663701f;
      sa[225] = 0.07524255f;
      sa[226] = -0.39658704f;
      sa[227] = 0.033137366f;
      sa[228] = -0.13271998f;
      sa[229] = -0.26305768f;
      sa[230] = -0.1838089f;
      sa[231] = -0.43769884f;
      sa[232] = 0.49838316f;
      sa[233] = -0.15314724f;
      sa[234] = -0.32157248f;
      sa[235] = -0.036060285f;
      sa[236] = -0.37936378f;
      sa[237] = 0.46568432f;
      sa[238] = -0.33487377f;
      sa[239] = 0.09202558f;
      sa[240] = -0.23685548f;
      sa[241] = -0.5252257f;
      sa[242] = -0.3330868f;
      sa[243] = 0.50506836f;
      sa[244] = 0.100927085f;
      sa[245] = 0.22123048f;
      sa[246] = 0.31323513f;
      sa[247] = -0.56796384f;
      sa[248] = 0.27804464f;
      sa[249] = -0.15403302f;
      sa[250] = 0.05045739f;
      sa[251] = 0.49847707f;
      sa[252] = 0.0972765f;
      sa[253] = -0.071256205f;
      sa[254] = 0.37066278f;
      sa[255] = 0.31315747f;
    }
  }
}
// Neuron weights connecting Tanh and Softmax layer
class h2o_nn_16x16x6_Tanh_11_Weight_3 implements java.io.Serializable {
  public static final float[] VALUES = new float[96];
  static {
    h2o_nn_16x16x6_Tanh_11_Weight_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_Weight_3_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = -0.2861841f;
      sa[1] = -1.5459306f;
      sa[2] = -1.5369767f;
      sa[3] = 1.3339508f;
      sa[4] = 0.21264158f;
      sa[5] = -0.16444132f;
      sa[6] = -1.5063889f;
      sa[7] = 1.4388943f;
      sa[8] = 2.2198339f;
      sa[9] = 0.08639903f;
      sa[10] = 1.956038f;
      sa[11] = 1.1503769f;
      sa[12] = 1.5007908f;
      sa[13] = -1.17623f;
      sa[14] = -1.9206097f;
      sa[15] = -0.5615234f;
      sa[16] = 0.45551607f;
      sa[17] = -0.9158583f;
      sa[18] = -1.3376552f;
      sa[19] = -1.204435f;
      sa[20] = -1.8522495f;
      sa[21] = 0.16445825f;
      sa[22] = -1.9169891f;
      sa[23] = -0.8043495f;
      sa[24] = -0.37026665f;
      sa[25] = 1.9997612f;
      sa[26] = -0.29731345f;
      sa[27] = 1.2821735f;
      sa[28] = 1.2110518f;
      sa[29] = -0.91401166f;
      sa[30] = 0.30469722f;
      sa[31] = 0.39824393f;
      sa[32] = 0.394808f;
      sa[33] = 0.38317573f;
      sa[34] = -0.527238f;
      sa[35] = 0.9238419f;
      sa[36] = 0.43727973f;
      sa[37] = -1.342842f;
      sa[38] = 0.5249131f;
      sa[39] = -0.25074634f;
      sa[40] = 0.37976125f;
      sa[41] = -0.65544665f;
      sa[42] = 0.63074183f;
      sa[43] = -0.24475162f;
      sa[44] = 0.9149806f;
      sa[45] = -2.0616019f;
      sa[46] = -1.8664972f;
      sa[47] = 0.14645216f;
      sa[48] = 1.9380214f;
      sa[49] = -0.053008508f;
      sa[50] = -0.611932f;
      sa[51] = 1.8260099f;
      sa[52] = -1.6440954f;
      sa[53] = 0.890831f;
      sa[54] = 1.7005652f;
      sa[55] = 1.3002821f;
      sa[56] = -0.682498f;
      sa[57] = -0.020085288f;
      sa[58] = 0.9430623f;
      sa[59] = 0.8683517f;
      sa[60] = -1.302753f;
      sa[61] = 1.1568718f;
      sa[62] = -0.48714572f;
      sa[63] = -0.91919166f;
      sa[64] = 1.0856814f;
      sa[65] = -0.90718365f;
      sa[66] = -1.8362314f;
      sa[67] = 0.13072085f;
      sa[68] = 1.1402382f;
      sa[69] = -1.1943203f;
      sa[70] = -1.5241139f;
      sa[71] = -0.052245352f;
      sa[72] = -1.4010339f;
      sa[73] = -1.4288169f;
      sa[74] = -1.3748534f;
      sa[75] = 1.3574227f;
      sa[76] = 1.9248203f;
      sa[77] = -0.4391445f;
      sa[78] = -1.715689f;
      sa[79] = -0.88931537f;
      sa[80] = 0.7149446f;
      sa[81] = -1.5812167f;
      sa[82] = -0.1601239f;
      sa[83] = -1.5607926f;
      sa[84] = 1.591604f;
      sa[85] = 1.1770619f;
      sa[86] = -1.9958066f;
      sa[87] = 1.3013498f;
      sa[88] = -1.382664f;
      sa[89] = -1.021849f;
      sa[90] = 0.24427597f;
      sa[91] = 0.22826467f;
      sa[92] = 1.3224742f;
      sa[93] = 1.9789864f;
      sa[94] = 1.9209331f;
      sa[95] = 0.8123862f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_16x16x6_Tanh_11 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_16x16x6_Tanh_11_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_16x16x6_Tanh_11_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_11_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[6];
  static {
    h2o_nn_16x16x6_Tanh_11_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_11_ColInfo_13_0 implements java.io.Serializable {
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


