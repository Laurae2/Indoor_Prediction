/*
  Licensed under the Apache License, Version 2.0
    http://www.apache.org/licenses/LICENSE-2.0.html

  AUTOGENERATED BY H2O at 2016-12-26T14:41:31.264+01:00
  3.10.0.8
  
  Standalone prediction code with sample test data for DeepLearningModel named h2o_nn_16x16x6_Tanh_03

  How to download, compile and execute:
      mkdir tmpdir
      cd tmpdir
      curl http://127.0.0.1:54321/3/h2o-genmodel.jar > h2o-genmodel.jar
      curl http://127.0.0.1:54321/3/Models.java/h2o_nn_16x16x6_Tanh_03 > h2o_nn_16x16x6_Tanh_03.java
      javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=128m h2o_nn_16x16x6_Tanh_03.java

     (Note:  Try java argument -XX:+PrintCompilation to show runtime JIT compiler behavior.)
*/
import java.util.Map;
import hex.genmodel.GenModel;
import hex.genmodel.annotations.ModelPojo;

@ModelPojo(name="h2o_nn_16x16x6_Tanh_03", algorithm="deeplearning")
public class h2o_nn_16x16x6_Tanh_03 extends GenModel {
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
      /* Input */ h2o_nn_16x16x6_Tanh_03_Activation_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_03_Activation_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_03_Activation_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_03_Activation_3.VALUES
    };
    // Neuron bias values.
    public static final double[][] BIAS = new double[][] {
      /* Input */ h2o_nn_16x16x6_Tanh_03_Bias_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_03_Bias_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_03_Bias_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_03_Bias_3.VALUES
    };
    // Connecting weights between neurons.
    public static final float[][] WEIGHT = new float[][] {
      /* Input */ h2o_nn_16x16x6_Tanh_03_Weight_0.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_03_Weight_1.VALUES,
      /* Tanh */ h2o_nn_16x16x6_Tanh_03_Weight_2.VALUES,
      /* Softmax */ h2o_nn_16x16x6_Tanh_03_Weight_3.VALUES
    };

  // Names of columns used by model.
  public static final String[] NAMES = NamesHolder_h2o_nn_16x16x6_Tanh_03.VALUES;
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
    /* Label */ h2o_nn_16x16x6_Tanh_03_ColInfo_13.VALUES
  };
  // Prior class distribution
  public static final double[] PRIOR_CLASS_DISTRIB = {0.27956989247311825,0.13978494623655913,0.15053763440860216,0.27956989247311825,0.15053763440860216};
  // Class distribution used for model building
  public static final double[] MODEL_CLASS_DISTRIB = null;

  public h2o_nn_16x16x6_Tanh_03() { super(NAMES,DOMAINS); }
  public String getUUID() { return Long.toString(8211600592341638656L); }

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
class h2o_nn_16x16x6_Tanh_03_Activation_0 implements java.io.Serializable {
  public static final double[] VALUES = new double[13];
  static {
    h2o_nn_16x16x6_Tanh_03_Activation_0_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_Activation_0_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_03_Activation_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_03_Activation_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_Activation_1_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_03_Activation_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_03_Activation_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_Activation_2_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_03_Activation_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_16x16x6_Tanh_03_Activation_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_Activation_3_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_03_Bias_0 implements java.io.Serializable {
  public static final double[] VALUES = null;
}
// Neuron bias values for Tanh layer
class h2o_nn_16x16x6_Tanh_03_Bias_1 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_03_Bias_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_Bias_1_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.009287315121349308;
      sa[1] = 0.042189801136289215;
      sa[2] = 0.00908710843811715;
      sa[3] = 0.01697734283219232;
      sa[4] = 0.0805567985649095;
      sa[5] = 0.021595507043259354;
      sa[6] = 0.03185827978436362;
      sa[7] = -0.053561345561714345;
      sa[8] = -0.04181143815948887;
      sa[9] = 0.01099554608399477;
      sa[10] = 0.026286895290617972;
      sa[11] = -0.027029591126868736;
      sa[12] = 0.030728320678878072;
      sa[13] = 0.005437536956301335;
      sa[14] = -0.027111186206452895;
      sa[15] = 0.03235344439961116;
    }
  }
}
// Neuron bias values for Tanh layer
class h2o_nn_16x16x6_Tanh_03_Bias_2 implements java.io.Serializable {
  public static final double[] VALUES = new double[16];
  static {
    h2o_nn_16x16x6_Tanh_03_Bias_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_Bias_2_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.018491437047348205;
      sa[1] = 5.280954582307968E-4;
      sa[2] = -0.023809435221425177;
      sa[3] = 0.013207023643249494;
      sa[4] = 0.0295651926160496;
      sa[5] = -0.02604764407412265;
      sa[6] = -0.028154900693654113;
      sa[7] = 0.019880342204748734;
      sa[8] = 0.029424026283998383;
      sa[9] = -0.021215244164588396;
      sa[10] = 0.0688095433789515;
      sa[11] = 0.010649407589068781;
      sa[12] = 0.061066022577047005;
      sa[13] = -0.03292763283180611;
      sa[14] = -0.0806233474615528;
      sa[15] = 0.07615780297547332;
    }
  }
}
// Neuron bias values for Softmax layer
class h2o_nn_16x16x6_Tanh_03_Bias_3 implements java.io.Serializable {
  public static final double[] VALUES = new double[5];
  static {
    h2o_nn_16x16x6_Tanh_03_Bias_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_Bias_3_0 implements java.io.Serializable {
    static final void fill(double[] sa) {
      sa[0] = 0.024410450405035918;
      sa[1] = -0.05039364343865685;
      sa[2] = -0.010940247177054102;
      sa[3] = -0.028174778984827613;
      sa[4] = -0.06916821133624936;
    }
  }
}
class h2o_nn_16x16x6_Tanh_03_Weight_0 implements java.io.Serializable {
  public static final float[] VALUES = null;
}
// Neuron weights connecting Input and Tanh layer
class h2o_nn_16x16x6_Tanh_03_Weight_1 implements java.io.Serializable {
  public static final float[] VALUES = new float[208];
  static {
    h2o_nn_16x16x6_Tanh_03_Weight_1_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_Weight_1_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.10561764f;
      sa[1] = 0.3676829f;
      sa[2] = 0.27990863f;
      sa[3] = 0.3427518f;
      sa[4] = -0.06981795f;
      sa[5] = -0.33113235f;
      sa[6] = -0.05057024f;
      sa[7] = 0.3736671f;
      sa[8] = 0.17806584f;
      sa[9] = -0.29035312f;
      sa[10] = -0.081792064f;
      sa[11] = -0.27523395f;
      sa[12] = 0.07693868f;
      sa[13] = -0.29950073f;
      sa[14] = -0.37148538f;
      sa[15] = -0.014172772f;
      sa[16] = -0.005875914f;
      sa[17] = 0.19216394f;
      sa[18] = -0.31546026f;
      sa[19] = 0.11229235f;
      sa[20] = 0.36217207f;
      sa[21] = -0.3786422f;
      sa[22] = 0.37351316f;
      sa[23] = -0.10810397f;
      sa[24] = 0.48976302f;
      sa[25] = 0.21199918f;
      sa[26] = -0.47727197f;
      sa[27] = 0.4585748f;
      sa[28] = 0.36065796f;
      sa[29] = -0.45937008f;
      sa[30] = -0.41494584f;
      sa[31] = 0.31952944f;
      sa[32] = 0.29090273f;
      sa[33] = -0.4455994f;
      sa[34] = 0.13353716f;
      sa[35] = -0.047236815f;
      sa[36] = -0.07982315f;
      sa[37] = 0.35534474f;
      sa[38] = -0.17489588f;
      sa[39] = 0.229853f;
      sa[40] = -0.048902225f;
      sa[41] = -0.07513182f;
      sa[42] = -0.12110166f;
      sa[43] = 0.16790996f;
      sa[44] = 0.2222564f;
      sa[45] = -0.036533467f;
      sa[46] = 0.008223564f;
      sa[47] = -0.030650735f;
      sa[48] = 0.25429323f;
      sa[49] = 0.528396f;
      sa[50] = 0.30379528f;
      sa[51] = -0.56046337f;
      sa[52] = -0.13467701f;
      sa[53] = 0.26104334f;
      sa[54] = 0.36913678f;
      sa[55] = 0.059261378f;
      sa[56] = -0.21691279f;
      sa[57] = -0.48306674f;
      sa[58] = 0.6785233f;
      sa[59] = -0.27778143f;
      sa[60] = -0.44373542f;
      sa[61] = 0.22202705f;
      sa[62] = 0.14763333f;
      sa[63] = -0.019834926f;
      sa[64] = 0.082503326f;
      sa[65] = 0.11019895f;
      sa[66] = 0.12373691f;
      sa[67] = 0.24211209f;
      sa[68] = 0.089282244f;
      sa[69] = 0.5241332f;
      sa[70] = -0.40983295f;
      sa[71] = 0.6605355f;
      sa[72] = -0.19779813f;
      sa[73] = 0.21421877f;
      sa[74] = -0.3209663f;
      sa[75] = 0.10221774f;
      sa[76] = -0.38509566f;
      sa[77] = -0.10094391f;
      sa[78] = -0.10373629f;
      sa[79] = 0.29352498f;
      sa[80] = -0.28919226f;
      sa[81] = -0.12876526f;
      sa[82] = -0.03721243f;
      sa[83] = 0.1782993f;
      sa[84] = -0.06894987f;
      sa[85] = -0.23744123f;
      sa[86] = 0.2250526f;
      sa[87] = 0.3863585f;
      sa[88] = 0.2565367f;
      sa[89] = 0.033861257f;
      sa[90] = -0.13351025f;
      sa[91] = 0.1620496f;
      sa[92] = 0.15157631f;
      sa[93] = -0.31683463f;
      sa[94] = -0.18219389f;
      sa[95] = -0.30358934f;
      sa[96] = 0.1277359f;
      sa[97] = 0.29427683f;
      sa[98] = 0.3061471f;
      sa[99] = -0.17680292f;
      sa[100] = -0.13648032f;
      sa[101] = -0.2424789f;
      sa[102] = -0.50143325f;
      sa[103] = 0.0058759064f;
      sa[104] = 0.04461604f;
      sa[105] = -0.13414726f;
      sa[106] = -0.34568518f;
      sa[107] = 0.0901298f;
      sa[108] = 0.21202935f;
      sa[109] = -0.26312903f;
      sa[110] = -0.4729621f;
      sa[111] = 0.21007553f;
      sa[112] = 0.20761533f;
      sa[113] = 0.12000001f;
      sa[114] = -0.2686724f;
      sa[115] = 0.052888192f;
      sa[116] = -0.47316125f;
      sa[117] = 0.01572826f;
      sa[118] = -0.47439912f;
      sa[119] = -0.27894706f;
      sa[120] = 0.27383158f;
      sa[121] = 0.3722455f;
      sa[122] = 0.38141903f;
      sa[123] = 0.4640805f;
      sa[124] = 0.3843752f;
      sa[125] = 0.41561154f;
      sa[126] = -0.43211585f;
      sa[127] = -0.19034691f;
      sa[128] = 0.1491722f;
      sa[129] = 0.20936218f;
      sa[130] = -0.23395614f;
      sa[131] = 0.46732777f;
      sa[132] = 0.28254288f;
      sa[133] = -0.3882931f;
      sa[134] = 0.1054757f;
      sa[135] = -0.3578859f;
      sa[136] = -0.27204776f;
      sa[137] = -0.0313481f;
      sa[138] = 0.12262019f;
      sa[139] = -0.14884928f;
      sa[140] = -0.29189268f;
      sa[141] = -0.27703616f;
      sa[142] = 0.21593744f;
      sa[143] = -0.4536338f;
      sa[144] = 0.44647786f;
      sa[145] = -0.21330471f;
      sa[146] = -0.36336508f;
      sa[147] = 0.23618425f;
      sa[148] = 0.27850223f;
      sa[149] = 0.32072043f;
      sa[150] = 0.4224779f;
      sa[151] = -0.38411036f;
      sa[152] = 0.053034663f;
      sa[153] = 0.31411135f;
      sa[154] = 0.2662101f;
      sa[155] = -0.20388943f;
      sa[156] = 0.1700751f;
      sa[157] = -0.3187367f;
      sa[158] = 0.14575826f;
      sa[159] = 0.1293312f;
      sa[160] = 0.053452995f;
      sa[161] = 0.31746477f;
      sa[162] = -0.14083487f;
      sa[163] = -0.1105767f;
      sa[164] = 0.34013152f;
      sa[165] = -0.06754362f;
      sa[166] = -0.2150251f;
      sa[167] = -0.20382944f;
      sa[168] = 0.27895418f;
      sa[169] = 0.43325752f;
      sa[170] = -0.03422863f;
      sa[171] = -0.04610341f;
      sa[172] = -0.12499161f;
      sa[173] = -0.25589734f;
      sa[174] = -0.01909653f;
      sa[175] = -0.2896217f;
      sa[176] = -0.08589221f;
      sa[177] = -0.066917606f;
      sa[178] = -0.03201093f;
      sa[179] = 0.13394457f;
      sa[180] = 0.045363575f;
      sa[181] = -0.52415353f;
      sa[182] = -0.21585944f;
      sa[183] = 0.18691887f;
      sa[184] = 0.42996654f;
      sa[185] = 0.34735328f;
      sa[186] = -0.33893925f;
      sa[187] = 0.017502742f;
      sa[188] = -0.008273232f;
      sa[189] = 0.32511562f;
      sa[190] = 0.031155735f;
      sa[191] = -0.0928507f;
      sa[192] = -0.21924616f;
      sa[193] = 0.273608f;
      sa[194] = 0.12911728f;
      sa[195] = 0.41178915f;
      sa[196] = 0.3324488f;
      sa[197] = -0.026354909f;
      sa[198] = 0.1844442f;
      sa[199] = 0.016411083f;
      sa[200] = -0.05702851f;
      sa[201] = -0.30395538f;
      sa[202] = 0.19152457f;
      sa[203] = -0.04968256f;
      sa[204] = -0.3295115f;
      sa[205] = 0.5384122f;
      sa[206] = 0.3348287f;
      sa[207] = 0.19929543f;
    }
  }
}
// Neuron weights connecting Tanh and Tanh layer
class h2o_nn_16x16x6_Tanh_03_Weight_2 implements java.io.Serializable {
  public static final float[] VALUES = new float[256];
  static {
    h2o_nn_16x16x6_Tanh_03_Weight_2_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_Weight_2_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.3879875f;
      sa[1] = -0.11032586f;
      sa[2] = -0.22938654f;
      sa[3] = -0.31028995f;
      sa[4] = 0.02327726f;
      sa[5] = 0.052693967f;
      sa[6] = 0.30912367f;
      sa[7] = -0.12358902f;
      sa[8] = -0.38829815f;
      sa[9] = 0.20771252f;
      sa[10] = 0.040649235f;
      sa[11] = 0.03491406f;
      sa[12] = 0.16798124f;
      sa[13] = -0.05146863f;
      sa[14] = -0.37780142f;
      sa[15] = 0.4008466f;
      sa[16] = 0.5337781f;
      sa[17] = -0.16294992f;
      sa[18] = -0.08043672f;
      sa[19] = -0.23555078f;
      sa[20] = 0.38513476f;
      sa[21] = 0.12686956f;
      sa[22] = -0.35840973f;
      sa[23] = 0.29132473f;
      sa[24] = -0.4301961f;
      sa[25] = 0.18765917f;
      sa[26] = 0.023904735f;
      sa[27] = 0.32069206f;
      sa[28] = -0.27743366f;
      sa[29] = -0.22178152f;
      sa[30] = 0.06720206f;
      sa[31] = -0.22769512f;
      sa[32] = -0.19741502f;
      sa[33] = -0.46948275f;
      sa[34] = -0.06849625f;
      sa[35] = 0.24458538f;
      sa[36] = -0.039544523f;
      sa[37] = 0.0354078f;
      sa[38] = -0.03711886f;
      sa[39] = 0.4053127f;
      sa[40] = -0.3110149f;
      sa[41] = -0.30260158f;
      sa[42] = -0.17174688f;
      sa[43] = -0.47647008f;
      sa[44] = 0.11790455f;
      sa[45] = -0.4574546f;
      sa[46] = 0.3246448f;
      sa[47] = 0.26957592f;
      sa[48] = -0.25782254f;
      sa[49] = -0.31320396f;
      sa[50] = 0.16182548f;
      sa[51] = -0.2173741f;
      sa[52] = 0.24429077f;
      sa[53] = 0.22618791f;
      sa[54] = -0.18241999f;
      sa[55] = 0.50215524f;
      sa[56] = -0.33777878f;
      sa[57] = -0.26506686f;
      sa[58] = 0.11870052f;
      sa[59] = -0.15932003f;
      sa[60] = 0.41497928f;
      sa[61] = -0.18927568f;
      sa[62] = -0.030951843f;
      sa[63] = -0.066823244f;
      sa[64] = 0.35002005f;
      sa[65] = -0.118316755f;
      sa[66] = 0.19419187f;
      sa[67] = 0.45663837f;
      sa[68] = 0.24831705f;
      sa[69] = -0.17407273f;
      sa[70] = -0.42878613f;
      sa[71] = -0.13670635f;
      sa[72] = -0.30440697f;
      sa[73] = 0.36763045f;
      sa[74] = 0.2635567f;
      sa[75] = 0.025387654f;
      sa[76] = 0.16223817f;
      sa[77] = 0.16169958f;
      sa[78] = -0.37306696f;
      sa[79] = 0.13718045f;
      sa[80] = -0.19510338f;
      sa[81] = 0.027162066f;
      sa[82] = 0.28483593f;
      sa[83] = -0.19127817f;
      sa[84] = -0.54543674f;
      sa[85] = -0.6095589f;
      sa[86] = 0.122898966f;
      sa[87] = 0.41205603f;
      sa[88] = -0.4498685f;
      sa[89] = -0.2452724f;
      sa[90] = -0.1904777f;
      sa[91] = 0.3645246f;
      sa[92] = -0.008154556f;
      sa[93] = 0.08476885f;
      sa[94] = -0.30284578f;
      sa[95] = 0.015759446f;
      sa[96] = -0.18640837f;
      sa[97] = -0.0286092f;
      sa[98] = -0.08933728f;
      sa[99] = -0.055412833f;
      sa[100] = -0.27940154f;
      sa[101] = -0.039898362f;
      sa[102] = 0.267193f;
      sa[103] = 0.22850974f;
      sa[104] = -0.16527466f;
      sa[105] = -0.088782564f;
      sa[106] = 0.4989518f;
      sa[107] = 0.19172268f;
      sa[108] = 0.30129826f;
      sa[109] = -0.07579179f;
      sa[110] = 0.17005436f;
      sa[111] = -0.30017662f;
      sa[112] = -0.3429197f;
      sa[113] = 0.02839536f;
      sa[114] = 0.20956625f;
      sa[115] = -0.08070065f;
      sa[116] = 0.29178396f;
      sa[117] = -0.1156242f;
      sa[118] = -0.33289334f;
      sa[119] = -0.07819459f;
      sa[120] = -0.14483185f;
      sa[121] = 0.27679196f;
      sa[122] = -0.04486806f;
      sa[123] = 0.06941088f;
      sa[124] = -0.18617791f;
      sa[125] = 0.24338046f;
      sa[126] = -0.15963858f;
      sa[127] = -0.09611924f;
      sa[128] = -0.26916334f;
      sa[129] = 0.2297552f;
      sa[130] = 0.008791577f;
      sa[131] = 0.27390185f;
      sa[132] = 0.10296888f;
      sa[133] = 0.104555964f;
      sa[134] = 0.19087425f;
      sa[135] = -0.23052445f;
      sa[136] = 0.2977824f;
      sa[137] = -0.19895199f;
      sa[138] = -0.085802495f;
      sa[139] = 0.3463277f;
      sa[140] = -0.20944314f;
      sa[141] = 0.0594357f;
      sa[142] = 0.15247792f;
      sa[143] = -0.39536119f;
      sa[144] = 0.21561286f;
      sa[145] = -0.18418886f;
      sa[146] = 0.20662513f;
      sa[147] = 0.0040826295f;
      sa[148] = -0.3435415f;
      sa[149] = 0.0860158f;
      sa[150] = -0.15675423f;
      sa[151] = -0.5180676f;
      sa[152] = 0.54631567f;
      sa[153] = -0.25029132f;
      sa[154] = 0.39343712f;
      sa[155] = -0.3070326f;
      sa[156] = -0.40573037f;
      sa[157] = -0.23940015f;
      sa[158] = 0.050355718f;
      sa[159] = -0.0627858f;
      sa[160] = 0.39124665f;
      sa[161] = -0.32228288f;
      sa[162] = 0.5447712f;
      sa[163] = -0.21776554f;
      sa[164] = 0.16413504f;
      sa[165] = -0.27860475f;
      sa[166] = 0.3371113f;
      sa[167] = -0.21426512f;
      sa[168] = 0.3896471f;
      sa[169] = -0.4659861f;
      sa[170] = 0.31617463f;
      sa[171] = 0.21000145f;
      sa[172] = 0.40075722f;
      sa[173] = 0.46031088f;
      sa[174] = 0.06431185f;
      sa[175] = -0.3465804f;
      sa[176] = -0.32245216f;
      sa[177] = 0.058145985f;
      sa[178] = -0.1747729f;
      sa[179] = 0.033070497f;
      sa[180] = -0.5106003f;
      sa[181] = -0.38441327f;
      sa[182] = 0.17334715f;
      sa[183] = -0.35619268f;
      sa[184] = 0.39357963f;
      sa[185] = 0.24408479f;
      sa[186] = -0.1991059f;
      sa[187] = -0.21268359f;
      sa[188] = -0.15979117f;
      sa[189] = -0.09984955f;
      sa[190] = -0.35452983f;
      sa[191] = -0.18239346f;
      sa[192] = -0.12289651f;
      sa[193] = 0.18546283f;
      sa[194] = -0.23730598f;
      sa[195] = -0.14281946f;
      sa[196] = 0.14321318f;
      sa[197] = -0.19837382f;
      sa[198] = 0.04830565f;
      sa[199] = 0.30020142f;
      sa[200] = 0.073460594f;
      sa[201] = 0.21376324f;
      sa[202] = 0.06419782f;
      sa[203] = -0.30925867f;
      sa[204] = 0.046927862f;
      sa[205] = 0.4589706f;
      sa[206] = -0.13343139f;
      sa[207] = 0.14022256f;
      sa[208] = -0.20928852f;
      sa[209] = 0.13342483f;
      sa[210] = -0.10307239f;
      sa[211] = -0.31069902f;
      sa[212] = -0.41891903f;
      sa[213] = -0.12752308f;
      sa[214] = -0.3052457f;
      sa[215] = 0.019807667f;
      sa[216] = 0.024862079f;
      sa[217] = 0.3410319f;
      sa[218] = 0.09855218f;
      sa[219] = 0.047112737f;
      sa[220] = 0.23322651f;
      sa[221] = -0.08685856f;
      sa[222] = -0.25679505f;
      sa[223] = -0.26287448f;
      sa[224] = 0.28486472f;
      sa[225] = 0.04575029f;
      sa[226] = -0.064000495f;
      sa[227] = 0.0479471f;
      sa[228] = -0.17942302f;
      sa[229] = -0.31573385f;
      sa[230] = -0.20852253f;
      sa[231] = -0.1689445f;
      sa[232] = 0.3670446f;
      sa[233] = -0.20970364f;
      sa[234] = -0.06481316f;
      sa[235] = 0.021652851f;
      sa[236] = -0.285655f;
      sa[237] = 0.38939667f;
      sa[238] = 0.16261417f;
      sa[239] = 0.079020016f;
      sa[240] = -0.055214904f;
      sa[241] = -0.36538762f;
      sa[242] = -0.44196662f;
      sa[243] = 0.42776042f;
      sa[244] = 0.2129364f;
      sa[245] = 0.50362635f;
      sa[246] = 0.17927285f;
      sa[247] = -0.46635258f;
      sa[248] = -0.016457718f;
      sa[249] = -0.027831389f;
      sa[250] = 0.208948f;
      sa[251] = 0.4791337f;
      sa[252] = 0.14991085f;
      sa[253] = -0.30322808f;
      sa[254] = 0.29401183f;
      sa[255] = 0.3467267f;
    }
  }
}
// Neuron weights connecting Tanh and Softmax layer
class h2o_nn_16x16x6_Tanh_03_Weight_3 implements java.io.Serializable {
  public static final float[] VALUES = new float[80];
  static {
    h2o_nn_16x16x6_Tanh_03_Weight_3_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_Weight_3_0 implements java.io.Serializable {
    static final void fill(float[] sa) {
      sa[0] = 0.05821207f;
      sa[1] = -1.5738672f;
      sa[2] = -1.5845579f;
      sa[3] = 1.2281299f;
      sa[4] = 0.37677667f;
      sa[5] = -0.15222357f;
      sa[6] = -1.6290923f;
      sa[7] = 1.5489489f;
      sa[8] = 1.8912219f;
      sa[9] = 0.18120435f;
      sa[10] = 1.7991571f;
      sa[11] = 1.5090334f;
      sa[12] = 1.8773408f;
      sa[13] = -0.6265576f;
      sa[14] = -1.8455554f;
      sa[15] = -0.8131585f;
      sa[16] = 0.71549046f;
      sa[17] = -0.8683473f;
      sa[18] = -1.5210984f;
      sa[19] = -1.0743933f;
      sa[20] = -1.8772964f;
      sa[21] = 0.34210733f;
      sa[22] = -1.9588778f;
      sa[23] = -0.65540844f;
      sa[24] = -0.43588588f;
      sa[25] = 1.8882557f;
      sa[26] = -0.28363734f;
      sa[27] = 1.2806742f;
      sa[28] = 1.1282607f;
      sa[29] = -0.86288804f;
      sa[30] = 0.24803515f;
      sa[31] = 0.32187513f;
      sa[32] = 0.5873672f;
      sa[33] = 0.6086993f;
      sa[34] = -0.7852002f;
      sa[35] = 1.1053871f;
      sa[36] = 0.52804524f;
      sa[37] = -1.515348f;
      sa[38] = 0.48696497f;
      sa[39] = -0.16586602f;
      sa[40] = 0.19113858f;
      sa[41] = -0.6813516f;
      sa[42] = 0.39958316f;
      sa[43] = -0.1356782f;
      sa[44] = 1.0394168f;
      sa[45] = -1.9379392f;
      sa[46] = -2.1655138f;
      sa[47] = 0.021906205f;
      sa[48] = 1.7062806f;
      sa[49] = -0.17310172f;
      sa[50] = -0.44820747f;
      sa[51] = 1.8950585f;
      sa[52] = -1.6836106f;
      sa[53] = 0.96243066f;
      sa[54] = 1.8446938f;
      sa[55] = 1.0359864f;
      sa[56] = -0.6473783f;
      sa[57] = -0.049075875f;
      sa[58] = 1.047839f;
      sa[59] = 0.8165474f;
      sa[60] = -1.3353443f;
      sa[61] = 1.0241697f;
      sa[62] = -0.47258702f;
      sa[63] = -0.9772558f;
      sa[64] = 1.0897099f;
      sa[65] = -1.0599072f;
      sa[66] = -1.767623f;
      sa[67] = -0.13895108f;
      sa[68] = 0.93124825f;
      sa[69] = -1.3801992f;
      sa[70] = -1.7249624f;
      sa[71] = 0.08875111f;
      sa[72] = -0.9503716f;
      sa[73] = -1.2613889f;
      sa[74] = -1.3343219f;
      sa[75] = 1.3309925f;
      sa[76] = 1.8076483f;
      sa[77] = -0.6318537f;
      sa[78] = -1.5781841f;
      sa[79] = -0.22561902f;
    }
  }
}
// The class representing training column names
class NamesHolder_h2o_nn_16x16x6_Tanh_03 implements java.io.Serializable {
  public static final String[] VALUES = new String[13];
  static {
    NamesHolder_h2o_nn_16x16x6_Tanh_03_0.fill(VALUES);
  }
  static final class NamesHolder_h2o_nn_16x16x6_Tanh_03_0 implements java.io.Serializable {
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
class h2o_nn_16x16x6_Tanh_03_ColInfo_13 implements java.io.Serializable {
  public static final String[] VALUES = new String[5];
  static {
    h2o_nn_16x16x6_Tanh_03_ColInfo_13_0.fill(VALUES);
  }
  static final class h2o_nn_16x16x6_Tanh_03_ColInfo_13_0 implements java.io.Serializable {
    static final void fill(String[] sa) {
      sa[0] = "1";
      sa[1] = "2";
      sa[2] = "4";
      sa[3] = "5";
      sa[4] = "6";
    }
  }
}


