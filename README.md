**🎯 Chủ đề phân tích**

Tai nạn giao thông là một trong những nguyên nhân hàng đầu gây tử vong và thương tật, đặc biệt ở nhóm trẻ từ 12 đến 18 tuổi. Đây là độ tuổi có nhiều nguy cơ do thường xuyên sử dụng xe đạp, xe máy, hoặc đi bộ đến trường nhưng lại thiếu kiến thức và kỹ năng an toàn giao thông.

Với mong muốn làm rõ thực trạng và nguyên nhân của các ca TNGT trong nhóm tuổi này, nhóm lựa chọn chủ đề: “Tai nạn giao thông ở trẻ từ 12 đến 18 tuổi” để nghiên cứu và phân tích.

**📦 Bộ dữ liệu sử dụng**

Nhóm sử dụng 3 file dữ liệu y tế:

-   *Admission.xlsx:*

    -   Thông tin nhập viện: tuổi (age_years)

    -   LOS, loại nhập viện (admtype)

    -   Nguồn nhập viện (admsource)

    -   Phương thức ra viện (sepmode)

    -   Loại hình chăm sóc (caretype)

-   *Diagnosis.xlsx:*

    -   Mã nhập viện (admission_id) liên kết với file Admission.xlsx

    -   Mã chẩn đoán ICD-10

    -   Phân loại chẩn đoán (P, A, C, M)

-   *ICD10.xlsx:*

    -   Mã chẩn đoán

    -   Diễn giải tên bệnh theo mã ICD-10

**🧠 Cách khai thác dữ liệu**

Dữ liệu được chia làm 9 phần chính:

-   Top 10 mã TNGT phổ biến nhất (V01-V99) ở nhóm 12–18 tuổi

-   Thống kê số ngày nằm viện (LOS) theo từng mã TNGT

-   5 chẩn đoán chính (P) thường đi kèm khi có TNGT

-   Tần suất mắc TNGT theo giới tính và nhóm tuổi

-   So sánh tỷ lệ chấn thương giữa các nhóm tuổi (Chi-squared)

-   Phân bố loại nhập viện (admtype)

-   Phân bố phương thức ra viện (sepmode)

-   Phân bố nguồn nhập viện (admsource)

-   LOS trung bình theo nguồn và loại nhập viện

**📌 Một số phát hiện đáng chú ý**

-   *Tai nạn xe đạp là nguyên nhân phổ biến nhất.*

-   *Gãy xương chi trên thường gặp ở nhóm 12–14 tuổi.*

-   *Chấn thương sọ não tăng mạnh ở nhóm 17–18 tuổi.*

-   *Người đi bộ bị va chạm (V031) có nguy cơ tử vong cao.*

-   *Mã chẩn đoán không rõ (unknown) còn nhiều → cần cải thiện dữ liệu đầu vào.*

**📈 Các biểu đồ thống kê chính**

-   Biểu đồ 1: 5 mã ICD đi kèm phổ biến nhất (VD: U739, S0602, Y92xx...)

-   Biểu đồ 2: LOS theo từng mã TNGT (V0004 → V061)

-   Biểu đồ cột nhóm: Tần suất TNGT theo giới và nhóm tuổi

-   Biểu đồ tròn: Tình trạng ra viện, loại nhập viện, nguồn nhập viện

-   🧪 Kiểm định thống kê Chi-squared test: Cho thấy có sự khác biệt có ý nghĩa thống kê giữa nhóm tuổi và loại chấn thương chính (p \< 0.001).

Có thể bổ sung thêm:

Fisher’s Exact Test (khi số ca tử vong nhỏ)

Herfindahl–Hirschman Index (HHI) để đo mức độ tập trung nguồn nhập viện

ANOVA / Kruskal-Wallis để so sánh LOS theo nhóm

**🔍 Lưu ý cho người dùng dữ liệu**

1.  Cần hiểu bối cảnh và mục tiêu sử dụng: phân tích rủi ro, lập kế hoạch can thiệp, truyền thông an toàn giao thông.

2.  Đọc biểu đồ theo cấu trúc: tiêu đề → trục → màu sắc → nhấn mạnh số liệu nổi bật.

3.  Cẩn trọng khi diễn giải dữ liệu có số ca nhỏ (n \< 30).

4.  Kết hợp dữ liệu với chính sách y tế, giáo dục để tạo tác động thực tiễn.
