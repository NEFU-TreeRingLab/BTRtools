**其他语言版本：[English](README.md)**

# BTRtools

**这是BTRtools的Beta测试版，如果需要咨询R包使用方法或者运行中出现问题，请与我们联系。**

###### *详细的使用手册将在 R包稳定版开发后更新，短期内应该不会提供*

`BTRtools` 是基于 Shiny 开发的一个简单交互页面，用于简化BTR模型调参工作。

- Note：使用`BTRtools`前 请安装R包 `rBTR` 。[Link: rBTR](https://github.com/NEFU-TreeRingLab/rBTR)

## 安装

可以使用以下代码安装 rBTR 包:

```r
# Install rBTR package:
# devtools::install_github("NEFU-TreeRingLab/rBTR")

# Install BTRtools package:
# devtools::install_github("NEFU-TreeRingLab/BTRtools")
```

## 使用方法

##### `BTRtools` 提供了3个函数，对BTR模型进行逐步调参

##### Step 1 :  拟合树木生长的年龄趋势

  ```R
  # BTRtools::trend_age() 
  ```

##### Step2 ： 调整树木细胞（纤维细胞和导管）年内生长相关的参数

```r
# BTRtools::cell_growth()
```

##### Step 3 : 调整树木年际生长相关参数

```r
# BTRtools::calibrate_BTR()
```

###### Note: 模型调参需要的示例数据已在R包中提供，使用前需要导出为 ***csv*** 或者 ***Excel*** 文件

```r	
# Climate data input:
# data(Clims)
# write.csv(Clims, 'Clims.csv')
# write.xlsx(Clims, 'Clims.xlsx')

# Parameters:
# data(BPparam)
# write.csv(BPparam, 'BPparam.csv')
# write.xlsx(BPparam, 'BPparam.xlsx')

# Observed value of tree ring:
# data(BPring)
# write.csv(BPring, 'BPring.csv')
# write.xlsx(BPring, 'BPring.xlsx')

# Observed value of fiber cells:
# data(Cells)
# write.csv(Cells, 'Cells.csv')
# write.xlsx(Cells, 'Cells.xlsx')

# Observed value of vessels:
# data(Vessels)
# write.csv(Vessels, 'Vessels.csv')
# write.xlsx(Vessels, 'Vessels.xlsx')
```

