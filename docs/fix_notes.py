import re
import sys

filepath = r"c:\Degi\GitHub\wanulcas\docs\w_notes.md"
with open(filepath, "r", encoding="utf-8") as f:
    content = f.read()

# Separate the YAML part
marker = "example snippet of WaNuLCAS parameters formatted in YAML:"
idx = content.find(marker)
if idx == -1:
    print("Could not find YAML part")
    sys.exit(1)

idx_end = content.find("\n\n", idx)
main_text = content[:idx_end+2]

# Unwrapping main text paragraphs
blocks = main_text.split('\n\n')
new_blocks = []

for block in blocks:
    if not block.strip():
        continue
    # Keep code blocks, headers, quotes, tables, lists, image links, table/figure captions untouched
    # We will test start conditions
    first_line = block.lstrip()
    if (first_line.startswith('```') or 
        first_line.startswith('#') or 
        first_line.startswith('|') or 
        first_line.startswith('>') or 
        first_line.startswith('-') or 
        first_line.startswith('*') or 
        first_line.startswith('!') or 
        first_line.startswith('[') or 
        first_line.startswith('Figure') or 
        first_line.startswith('Table')):
        new_blocks.append(block)
    else:
        # Unwrap paragraphs
        # Join lines with space if they are not already joined
        lines = block.split('\n')
        joined = lines[0].strip()
        for line in lines[1:]:
            line = line.strip()
            if not line:
                continue
            if joined.endswith(' '):
                joined += line
            else:
                joined += ' ' + line
        new_blocks.append(joined)

main_text_fixed = '\n\n'.join(new_blocks)

yaml_replacement = """```yaml
vars:
  AF_AnyTrees_is: 1
  AF_Circ: 0
  AF_Crop_is: 1
  AF_DeepSubSoil: 3
  AF_DepthDynamic_is: 0
  AF_DepthGroundWater_Table: 0
  AF_DynPestImpacts_is: 0
  AF_PlotNumberUphill: 0
  EVAP_InitSlashM: 0.4
  EVAP_InitWoodM: 0.25
  EVAP_MulchEffSurfLit: 1
  N_Lat4InflowRelConc: 1
  N_LittNmin1exchfact: 0.1
  N_Use_NgassLossEst_is: 0
  T_DCanWidthMax: 22
  T_ExpRetThresh: 30
  T_GrowthResp: 1
arrays:
  crop_df:
    keys:
      crop_id: [1, 2, 3, 4, 5]
    vars:
      C_HostEffForT1: [0, 0, 0, 0, 0]
      C_LAIMax: [10, 5, 5, 5, 10]
      CQ_AgronYieldMoistFrac: [0.65, 0.15, 0.1, 0.1, 0.75]
      CQ_ClosedCanCurr: [0.2, 0.2, 0.2, 0.2, 7.7]
      CQ_GSeedCurr: [0.002, 0.004, 0.01, 0.01, 0.02]
      CQ_HBiomConvCurr: [5, 7, 1.5, 0.01, 0.7]
      CQ_kLightCurr: [0.58, 0.65, 0.6, 0.8, 0.5]
  layer_df:
    keys:
      layer: [1, 2, 3, 4]
    vars:
      AF_DepthLay: [0.2, 0.3, 0.25, 0.25]
      ClayLayer: [24.8, 20, 20.9, 20.9]
      GHG_AnaerobLayerW: [0.6, 0.2, 0.15, 0.05]
      MC2_kRelLayer: [1, 0.8, 0.7, 0.6]
      MC2_SOMDist: [1, 0.2, 0.1, 0.05]
      MN2_PassRelLayer: [1, 1.2, 1.4, 1.6]
      N_KaNH4: [5, 5, 5, 5]
graphs:
  CQ_CropType:
    type: continues
    x_var: CA_ComplCrop
    xy_data:
      CQ_CropType_Zn1:
        x_val: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
        y_val: [2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
      CQ_CropType_Zn2:
        x_val: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
        y_val: [2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2]
      CQ_CropType_Zn3:
        x_val: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
        y_val: [2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2]
      CQ_CropType_Zn4:
        x_val: [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
        y_val: [2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2]
  T_RestDaysperTappingday:
    type: continues
    x_var: T_DayofYear
    xy_data:
      T_RestDaysperTappingday:
        x_val: [1, 31.3, 61.7, 92, 122, 153, 183, 213, 244, 274, 304, 335, 365]
        y_val: [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
  T_SlashLabour:
    type: continues
    x_var: T_BiomassSlashed
    xy_data:
      T_SlashLabour:
        x_val: [0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
        y_val: [0, 26, 45, 61, 70, 80, 85, 89, 91, 93, 95]
  T_Temp:
    type: continues
    x_var: T_DayofYear
    xy_data:
      T_Temp:
        x_val: [0, 30.4, 60.8, 91.3, 122, 152, 183, 213, 243, 274, 304, 335, 365]
        y_val: [23.6, 22, 21.6, 22, 23.4, 25, 26.4, 27.4, 29.2, 29.2, 27.8, 25.4, 24]
  T_TempRespMaint:
    type: continues
    x_var: T_Temp
    xy_data:
      T_TempRespMaint:
        x_val: [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50]
        y_val: [0.225, 0.25, 0.3, 0.5, 0.725, 1, 1.43, 2, 2.92, 4, 4.97]
```"""

final_text = main_text_fixed + "\n" + yaml_replacement + "\n"

with open(filepath, "w", encoding="utf-8") as f:
    f.write(final_text)

print("Formatting applied successfully.")
